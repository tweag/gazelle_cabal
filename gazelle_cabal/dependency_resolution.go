// Functions used for dependency resolution
package gazelle_cabal

import (
	"errors"
	"flag"
	"fmt"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/language"
	golang "github.com/bazelbuild/bazel-gazelle/language/go"
	"github.com/bazelbuild/bazel-gazelle/language/proto"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"
	"github.com/bazelbuild/bazel-gazelle/walk"
	bzl "github.com/bazelbuild/buildtools/build"

	"log"
	"sort"
	"strings"
)

////////////////////////////////////////////////////////
// functions for dependency resolution in BUILD files
////////////////////////////////////////////////////////

type ImportData struct {
	Deps           ConfigurableList[string]
	GhcOpts        ConfigurableList[string]
	ExtraLibraries ConfigurableList[string]
	Tools          ConfigurableList[ToolName]
}

type ToolName struct {
	PackageName    string
	ExecutableName string
}

func setToolsAttribute(
	ix *resolve.RuleIndex,
	toolRepo string,
	r *rule.Rule,
	importData ImportData,
	from label.Label,
) {
	tools := make(ConfigurableList[string], 0, len(importData.Tools))
	for _, item := range importData.Tools {
		switch v := item.(type) {
		case []ToolName:
			for _, tool := range v {
				toolLabel := findToolLabel(ix, toolRepo, tool, from)
				tools.AddItem(toolLabel.String())
			}
		case map[string][]ToolName:
			result := make(rule.SelectStringListValue)
			for k, toolList := range v {
				if len(toolList) == 0 {
					result[k] = []string{}
					continue
				}
				for _, tool := range toolList {
					toolLabel := findToolLabel(ix, toolRepo, tool, from)
					addToMap(result, k, toolLabel.String())
				}
			}
			if len(result) > 0 {
				tools = append(tools, result)
			}
		default:
			log.Fatalf("unexpected ConfigurableList item type for tools: %T", v)
		}
	}

	SetAttrIfNotEmpty(r, "tools", tools)
}

// Drops preexisting macro definitions for tools. Adds new macro
// definitions for tools. Adds unresolved extra libraries as -llib
// flags.
func setCompilerFlagsAttribute(
	unresolvedExtraLibraries ConfigurableList[string],
	toolRepo string,
	ix *resolve.RuleIndex,
	r *rule.Rule,
	importData ImportData,
	from label.Label,
) {
	ghcOpts := make(ConfigurableList[string], 0,
		len(importData.ExtraLibraries)+len(importData.GhcOpts)+len(importData.Tools))

	ghcOpts = append(ghcOpts, dropToolMacroDefs(importData.GhcOpts, importData.Tools)...)
	ghcOpts = append(ghcOpts, addLibraryFlags(unresolvedExtraLibraries)...)
	ghcOpts = append(ghcOpts, addMacroDefs(ix, toolRepo, importData.Tools, from)...)

	SetAttrIfNotEmpty(r, "ghcopts", ghcOpts)
}

// Drops from xs all of the macro definitions for the given tools
func dropToolMacroDefs(xs ConfigurableList[string], tools ConfigurableList[ToolName]) ConfigurableList[string] {
	toolMacroDefs := collectToolMacroDefs(tools)

	return xs.MapFilter(
		func(s string) (string, bool) {
			// Keep strings that don't have any tool macro prefix
			if hasAPrefixIn(s, toolMacroDefs) {
				return "", false
			}
			return s, true
		},
	)
}

// collectToolMacroDefs extracts macro definition prefixes from tools.
func collectToolMacroDefs(tools ConfigurableList[ToolName]) []string {
	var toolMacroDefs []string
	for _, item := range tools {
		switch v := item.(type) {
		case []ToolName:
			for _, tool := range v {
				toolMacroDefs = append(toolMacroDefs, "-D"+toolMacroName(tool)+"=")
			}
		case map[string][]ToolName:
			for _, toolList := range v {
				for _, tool := range toolList {
					toolMacroDefs = append(toolMacroDefs, "-D"+toolMacroName(tool)+"=")
				}
			}
		default:
			log.Fatalf("unexpected ConfigurableList item type for tools: %T", v)
		}
	}
	return toolMacroDefs
}

// Adds macros defining the paths for the given tools
func addMacroDefs(
	ix *resolve.RuleIndex,
	toolRepo string,
	tools ConfigurableList[ToolName],
	from label.Label,
) ConfigurableList[string] {
	result := make(ConfigurableList[string], 0, len(tools))
	for _, item := range tools {
		switch v := item.(type) {
		case []ToolName:
			macros := make([]string, 0, len(v))
			for _, tool := range v {
				toolLabel := findToolLabel(ix, toolRepo, tool, from)
				macroDef := "-D" + toolMacroName(tool) + "=$(location " + toolLabel.String() + ")"
				macros = append(macros, macroDef)
			}
			if len(macros) > 0 {
				result = append(result, macros)
			}
		case map[string][]ToolName:
			selectResult := make(rule.SelectStringListValue)
			for k, toolList := range v {
				if len(toolList) == 0 {
					selectResult[k] = []string{}
					continue
				}
				for _, tool := range toolList {
					toolLabel := findToolLabel(ix, toolRepo, tool, from)
					macroDef := "-D" + toolMacroName(tool) + "=$(location " + toolLabel.String() + ")"
					addToMap(selectResult, k, macroDef)
				}
			}
			if len(selectResult) > 0 {
				result = append(result, selectResult)
			}
		default:
			log.Fatalf("unexpected ConfigurableList item type for tools: %T", v)
		}
	}
	return result
}

// Adds unresolved extra libraries as -llib flags.
func addLibraryFlags(unresolvedExtraLibraries ConfigurableList[string]) ConfigurableList[string] {
	return unresolvedExtraLibraries.Map(
		func(lib string) string {
			return "-l" + lib
		},
	)
}

// At least one of the strings in xs is a prefix of y
func hasAPrefixIn(y string, xs []string) bool {
	for _, x := range xs {
		if strings.HasPrefix(y, x) {
			return true
		}
	}
	return false
}

func toolMacroName(tool ToolName) string {
	toolString := tool.PackageName + "_" + tool.ExecutableName + "_PATH"
	return strings.ToUpper(strings.ReplaceAll(toolString, "-", "_"))
}

func findToolLabel(
	ix *resolve.RuleIndex,
	toolRepo string,
	tool ToolName,
	from label.Label,
) label.Label {
	toolString := tool.ExecutableName
	if tool.PackageName == tool.ExecutableName {
		toolString = toolString + "-binary"
	}

	spec := resolve.ImportSpec{Lang: gazelleCabalName, Imp: "exe:" + toolString}
	res := ix.FindRulesByImport(spec, gazelleCabalName)
	if len(res) > 0 {
		return rel(res[0].Label, from)
	} else {
		return rel(label.New(toolRepo, tool.PackageName, tool.ExecutableName), from)
	}
}

func rel(lbl label.Label, from label.Label) label.Label {
	return lbl.Rel(from.Repo, from.Pkg)
}

func addToMap[T any](m map[string][]T, key string, value T) {
	if _, exists := m[key]; exists {
		m[key] = append(m[key], value)
	} else {
		m[key] = []T{value}
	}
}

// All dependencies that are ghc_plugins are placed in the plugins
// attribute. All the dependencies that are indexed comming from
// haskell_library are assumed to be local. There rest of the
// dependencies are assumed to come from packageRepo.
func setDepsAndPluginsAttributes(
	extraLibraries ConfigurableList[label.Label],
	packageRepo string,
	ix *resolve.RuleIndex,
	r *rule.Rule,
	importData ImportData,
	from label.Label,
) {
	deps := make(ConfigurableList[string], 0, len(importData.Deps)+len(extraLibraries))
	plugins := make(ConfigurableList[string], 0, len(importData.Deps))

	for _, item := range importData.Deps {
		switch v := item.(type) {
		case []string:
			depsValue, pluginsValue := partitionDepsAndPlugins(v, ix, r, packageRepo, from)
			if len(pluginsValue) > 0 {
				plugins = append(plugins, pluginsValue)
			}
			if len(depsValue) > 0 {
				deps = append(deps, depsValue)
			}
		case rule.SelectStringListValue:
			depsSelect, pluginsSelect := partitionDepsAndPluginsSelect(v, ix, r, packageRepo, from)
			if len(pluginsSelect) > 0 {
				plugins = append(plugins, pluginsSelect)
			}
			if len(depsSelect) > 0 {
				deps = append(deps, depsSelect)
			}
		default:
			log.Fatalf("unexpected ConfigurableList item type: %T", v)
		}
	}

	appendExtraLibrariesToDeps(extraLibraries, &deps)
	SetAttrIfNotEmpty(r, "deps", deps)
	SetAttrIfNotEmpty(r, "plugins", plugins)
}

// partitionDepsAndPlugins separates dependencies into regular deps and plugins.
func partitionDepsAndPlugins(
	depNames []string,
	ix *resolve.RuleIndex,
	r *rule.Rule,
	packageRepo string,
	from label.Label,
) ([]string, []string) {
	depsValue := make([]string, 0, len(depNames))
	pluginsValue := make([]string, 0, len(depNames))

	for _, depName := range depNames {
		if plugin, err := getPluginLabel(ix, depName, from); err == nil {
			pluginsValue = append(pluginsValue, plugin.String())
		} else {
			haskellPkg := getPackageLabel(r, ix, packageRepo, depName, from)
			depsValue = append(depsValue, haskellPkg.String())
		}
	}
	return depsValue, pluginsValue
}

// partitionDepsAndPluginsSelect separates select dependencies into regular deps and plugins.
func partitionDepsAndPluginsSelect(
	sel rule.SelectStringListValue,
	ix *resolve.RuleIndex,
	r *rule.Rule,
	packageRepo string,
	from label.Label,
) (rule.SelectStringListValue, rule.SelectStringListValue) {
	var depsSelect, pluginsSelect rule.SelectStringListValue

	const defaultCondition = "//conditions:default"
	_, hasDefault := sel[defaultCondition]

	for k, depNames := range sel {
		if len(depNames) == 0 {
			if depsSelect == nil {
				depsSelect = make(rule.SelectStringListValue)
			}
			depsSelect[k] = []string{}
			continue
		}
		for _, depName := range depNames {
			if plugin, err := getPluginLabel(ix, depName, from); err == nil {
				if pluginsSelect == nil {
					pluginsSelect = make(rule.SelectStringListValue)
				}
				addToMap(pluginsSelect, k, plugin.String())
			} else {
				if depsSelect == nil {
					depsSelect = make(rule.SelectStringListValue)
				}
				haskellPkg := getPackageLabel(r, ix, packageRepo, depName, from)
				addToMap(depsSelect, k, haskellPkg.String())
			}
		}
	}

	// Ensure default condition is present in both outputs if it existed in the input
	if hasDefault {
		if depsSelect != nil && depsSelect[defaultCondition] == nil {
			depsSelect[defaultCondition] = []string{}
		}
		if pluginsSelect != nil && pluginsSelect[defaultCondition] == nil {
			pluginsSelect[defaultCondition] = []string{}
		}
	}

	return depsSelect, pluginsSelect
}

// appendExtraLibrariesToDeps adds extra library labels to the deps list.
func appendExtraLibrariesToDeps(extraLibraries ConfigurableList[label.Label], deps *ConfigurableList[string]) {
	for _, item := range extraLibraries {
		switch v := item.(type) {
		case label.Label:
			deps.AddItem(v.String())
		case map[string][]label.Label:
			log.Println("TODO (not implemented) " + fmt.Sprint(v))
		}
	}
}

// Yields the labels of the extra libraries and the names of the unresolved extra libraries
func getExtraLibraryLabels(c *config.Config, extraLibraries ConfigurableList[string]) (ConfigurableList[label.Label], ConfigurableList[string]) {
	foundExtraLibraryLabels := make(ConfigurableList[label.Label], 0, len(extraLibraries))
	unresolvedExtraLibraries := make(ConfigurableList[string], 0, len(extraLibraries))

	for _, item := range extraLibraries {
		switch v := item.(type) {
		case []string:
			found, unresolved := resolveLibraryList(c, v)
			if len(found) > 0 {
				foundExtraLibraryLabels = append(foundExtraLibraryLabels, found)
			}
			if len(unresolved) > 0 {
				unresolvedExtraLibraries = append(unresolvedExtraLibraries, unresolved)
			}
		case rule.SelectStringListValue:
			found, unresolved := resolveLibrarySelect(c, v)
			if len(found) > 0 {
				foundExtraLibraryLabels = append(foundExtraLibraryLabels, found)
			}
			if len(unresolved) > 0 {
				unresolvedExtraLibraries = append(unresolvedExtraLibraries, unresolved)
			}
		default:
			log.Fatalf("unexpected ConfigurableList item type: %T", v)
		}
	}

	return foundExtraLibraryLabels, unresolvedExtraLibraries
}

// resolveLibraryList resolves a list of library names to labels.
func resolveLibraryList(c *config.Config, libs []string) ([]label.Label, []string) {
	var foundValue []label.Label
	var unresolvedValue []string

	for _, lib := range libs {
		spec := resolve.ImportSpec{Lang: gazelleCabalName, Imp: lib}
		if libLabel, ok := resolve.FindRuleWithOverride(c, spec, gazelleCabalName); ok {
			foundValue = append(foundValue, libLabel)
		} else {
			unresolvedValue = append(unresolvedValue, lib)
		}
	}
	return foundValue, unresolvedValue
}

// resolveLibrarySelect resolves select library names to labels.
func resolveLibrarySelect(
	c *config.Config,
	sel rule.SelectStringListValue,
) (map[string][]label.Label, rule.SelectStringListValue) {
	var foundSelect map[string][]label.Label
	var unresolvedSelect rule.SelectStringListValue

	const defaultCondition = "//conditions:default"
	_, hasDefault := sel[defaultCondition]

	for k, libs := range sel {
		for _, lib := range libs {
			spec := resolve.ImportSpec{Lang: gazelleCabalName, Imp: lib}
			if libLabel, ok := resolve.FindRuleWithOverride(c, spec, gazelleCabalName); ok {
				if foundSelect == nil {
					foundSelect = make(map[string][]label.Label)
				}
				addToMap(foundSelect, k, libLabel)
			} else {
				if unresolvedSelect == nil {
					unresolvedSelect = make(rule.SelectStringListValue)
				}
				addToMap(unresolvedSelect, k, lib)
			}
		}
	}

	// Ensure default condition is present in both outputs if it existed in the input
	if hasDefault {
		if foundSelect != nil && foundSelect[defaultCondition] == nil {
			foundSelect[defaultCondition] = []label.Label{}
		}
		if unresolvedSelect != nil && unresolvedSelect[defaultCondition] == nil {
			unresolvedSelect[defaultCondition] = []string{}
		}
	}

	return foundSelect, unresolvedSelect
}

// Produces a plugin label if there is a ghc_plugin rule with the
// same name as the package name plus the "-plugin" suffix.
func getPluginLabel(
	ix *resolve.RuleIndex,
	packageName string,
	from label.Label,
) (label.Label, error) {
	spec := resolve.ImportSpec{Lang: gazelleCabalName, Imp: "ghc_plugin:" + packageName + "-plugin"}
	res := ix.FindRulesByImport(spec, gazelleCabalName)
	if len(res) > 0 {
		return rel(res[0].Label, from), nil
	} else {
		return label.Label{}, errors.New("Not a plugin: " + packageName)
	}
}

// Produces a label for the given package. We assume that if no rule
// is indexed with the package name, the package must come from packageRepo.
func getPackageLabel(
	r *rule.Rule,
	ix *resolve.RuleIndex,
	packageRepo string,
	pkgName string,
	from label.Label,
) label.Label {

	cabalPkgName := r.PrivateAttr("pkgName").(string)
	for _, searchScope := range getRuleIndexKeys(pkgName, from, cabalPkgName) {
		if labelFound, err := searchInLibraries(ix, packageRepo, pkgName, from, searchScope); err == nil {
			return labelFound
		}
	}

	// grab from repository
	return rel(label.New(packageRepo, "", pkgName), from)
}

// Produces the key to search the label of a library (either internal or public).
func getRuleIndexKeys(depName string, from label.Label, cabalPkgName string) []string {
	splitted := strings.Split(depName, ":")
	publicPrefix := "public_library"
	privatePrefix := "private_library"
	format := "%s:%s:%s"
	if len(splitted) <= 1 {
		// no colon prefix detected
		return []string{
			// it is a locally defined, named library which can be either
			fmt.Sprintf(format, privatePrefix, cabalPkgName, depName),
			fmt.Sprintf(format, publicPrefix, cabalPkgName, depName),
			// or it can be a public, main library from another package
			fmt.Sprintf(format, publicPrefix, depName, depName),
		}
	}

	// colon prefix detected
	packagePrefix := splitted[0]
	libraryName := splitted[1]
	if packagePrefix == cabalPkgName {
		// the package prefix leads to a sub-library of the same package
		return []string{
			fmt.Sprintf(format, privatePrefix, packagePrefix, libraryName),
			fmt.Sprintf(format, publicPrefix, packagePrefix, libraryName),
		}
	}

	// the prefix leads to a sub-library from another package
	return []string{
		fmt.Sprintf(format, publicPrefix, packagePrefix, libraryName),
	}
}

func searchInLibraries(
	ix *resolve.RuleIndex,
	packageRepo string,
	pkgName string,
	from label.Label,
	importId string,
) (label.Label, error) {
	spec := resolve.ImportSpec{Lang: gazelleCabalName, Imp: importId}
	res := ix.FindRulesByImport(spec, gazelleCabalName)

	librariesFound := len(res)
	if librariesFound > 1 {
		// There are at least two cabal pkgs with the same name
		log.Fatalf("Multiple labels found under %s for package %s : %s", from, pkgName, res)
	}
	// We take the dep we've found locally, if it's not a circular dependency
	if librariesFound == 1 {
		r := res[0]
		if r.IsSelfImport(from) {
			log.Fatalf("Dependency cycle detected in the following component: %s", from)
		}
		return rel(r.Label, from), nil
	}

	return label.Label{}, fmt.Errorf("Library '%s' referenced from '%s' not found, %s", pkgName, from, importId)
}

func SetAttrIfNotEmpty[T any, S ~[]T](r *rule.Rule, attr string, values S) {
	if len(values) > 0 {
		r.SetAttr(attr, values)
	}
}

///////////////////////////////////////////////////////////////////
// functions to resolve dependencies for the stack_snapshot rule
///////////////////////////////////////////////////////////////////

// Scans all BUILD files for Haskell rules and provides the list
// of Haskell packages and tools that they use.
func collectDependenciesFromRepo(
	c *config.Config,
	lang language.Language,
) ([]string, map[string][]string) {

	packages := map[string]bool{}
	tools := map[string]map[string]bool{}
	cexts := []config.Configurer{
		&config.CommonConfigurer{},
		&walk.Configurer{},
		lang,
		golang.NewLanguage(),
		proto.NewLanguage(),
	}

	initUpdateReposConfig(c, cexts)

	walk.Walk(
		c,
		cexts,
		[]string{},
		walk.VisitAllUpdateDirsMode,
		func(
			dir,
			rel string,
			c *config.Config,
			update bool,
			f *rule.File,
			subdirs,
			regularFiles,
			genFiles []string,
		) {
			collectDependenciesFromFile(f, c, packages, tools)
		},
	)
	packageList := mapSortedStringKeys(packages)
	components := toolsToComponents(tools)
	return packageList, components
}

func collectDependenciesFromFile(
	f *rule.File,
	c *config.Config,
	packages map[string]bool,
	tools map[string]map[string]bool,
) {
	if f == nil {
		return
	}
	packageRepo := c.Exts[gazelleCabalName].(Config).HaskellPackageRepo
	toolRepo := packageRepo + "-exe"

	for _, r := range f.Rules {
		if isHaskellRule(r.Kind()) {
			// Collect all dependency labels (from lists and selects)
			if depsAttr := r.Attr("deps"); depsAttr != nil {
				for labelString := range collectStringsFromExpr(depsAttr) {
					label, err := parseAbsoluteLabel(labelString)
					if err == nil && label.Repo == packageRepo && label.Pkg == "" {
						packages[label.Name] = true
					}
				}
			}

			// Collect all tool labels (from lists and selects)
			if toolsAttr := r.Attr("tools"); toolsAttr != nil {
				for labelString := range collectStringsFromExpr(toolsAttr) {
					label, err := parseAbsoluteLabel(labelString)
					if err == nil && label.Repo == toolRepo {
						m, ok := tools[label.Pkg]
						if !ok {
							m = make(map[string]bool)
							tools[label.Pkg] = m
						}
						m[label.Name] = true
						packages[label.Pkg] = true
					}
				}
			}
		}
	}
}

func toolsToComponents(tools map[string]map[string]bool) map[string][]string {
	toolMaps := make(map[string][]string)
	for toolPackageName, toolExecutableNames := range tools {
		comps := make([]string, len(toolExecutableNames)+1)
		comps[0] = "lib"
		i := 0
		for exeName := range toolExecutableNames {
			comps[i+1] = "exe:" + exeName
			i++
		}
		sort.Strings(comps[1:])
		toolMaps[toolPackageName] = comps
	}
	return toolMaps
}

func mapSortedStringKeys(m map[string]bool) []string {
	ss := make([]string, len(m))
	i := 0
	for s := range m {
		ss[i] = s
		i++
	}
	sort.Strings(ss)
	return ss
}

// label.Parse chokes on hyphenated repo names with
// label parse error: repository has invalid characters
// https://github.com/bazelbuild/bazel-gazelle/issues/1082
func parseAbsoluteLabel(v string) (label.Label, error) {
	repo := strings.Split(v, "//")
	if len(repo) > 1 && strings.HasPrefix(repo[0], "@") {
		pkg := strings.Split(repo[1], ":")
		if len(pkg) > 1 {
			return label.New(repo[0][1:], pkg[0], pkg[1]), nil
		} else {
			return label.New(repo[0][1:], pkg[0], pkg[0]), nil
		}
	}
	return label.Label{}, fmt.Errorf("Can't parse: %s", v)
}

// collectStringsFromExpr walks through a bzl.Expr tree and collects all string literals
func collectStringsFromExpr(expr bzl.Expr) map[string]bool {
	result := make(map[string]bool)
	walkExprForStrings(expr, func(s string) {
		result[s] = true
	})
	return result
}

// walkExprForStrings walks through a bzl.Expr tree and calls fn for each string literal
func walkExprForStrings(expr bzl.Expr, fn func(string)) {
	if expr == nil {
		return
	}

	switch e := expr.(type) {
	case *bzl.StringExpr:
		fn(e.Value)
	case *bzl.BinaryExpr:
		if e.Op == "+" {
			walkExprForStrings(e.X, fn)
			walkExprForStrings(e.Y, fn)
		}
	case *bzl.ListExpr:
		for _, elem := range e.List {
			walkExprForStrings(elem, fn)
		}
	case *bzl.CallExpr:
		// Handle select() expressions
		for _, arg := range e.List {
			walkExprForStrings(arg, fn)
		}
	case *bzl.DictExpr:
		// Inside select's dictionary - walk values
		for _, keyVal := range e.List {
			walkExprForStrings(keyVal.Value, fn)
		}
	}
}

func isHaskellRule(kind string) bool {
	return kind == "haskell_library" ||
		kind == "haskell_binary" ||
		kind == "haskell_test" ||
		kind == "ghc_plugin"
}

func initUpdateReposConfig(c *config.Config, cexts []config.Configurer) {
	fs := flag.NewFlagSet("updateReposFlagSet", flag.ContinueOnError)

	for _, cext := range cexts {
		cext.RegisterFlags(fs, "update", c)
	}

	for _, cext := range cexts {
		if err := cext.CheckFlags(fs, c); err != nil {
			log.Fatal(err)
		}
	}
}
