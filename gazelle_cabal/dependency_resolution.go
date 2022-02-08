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

	"log"
	"sort"
	"strings"
)

////////////////////////////////////////////////////////
// functions for dependency resolution in BUILD files
////////////////////////////////////////////////////////

type ImportData struct {
	Deps           []string
	CompilerFlags  []string
	ExtraLibraries []string
	Tools          []ToolName
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
	tools := make([]string, 0, len(importData.Tools))
	for _, tool := range importData.Tools {
		toolLabel := findToolLabel(ix, toolRepo, tool, from)
		tools = append(tools, toolLabel.String())
	}

	SetAttrIfNotEmpty(r, "tools", tools)
}

// Drops preexisting macro definitions for tools. Adds new macro
// definitions for tools. Adds unresolved extra libraries as -llib
// flags.
func setCompilerFlagsAttribute(
	extraLibrariesMap map[string]string,
	toolRepo string,
	ix *resolve.RuleIndex,
	r *rule.Rule,
	importData ImportData,
	from label.Label,
) {
	compilerFlagsSize :=
		len(importData.ExtraLibraries) +
			len(importData.CompilerFlags) +
			len(importData.Tools)
	compilerFlags := make([]string, 0, compilerFlagsSize)

	dropToolMacroDefs(importData.CompilerFlags, importData.Tools, &compilerFlags)
	addLibraryFlags(extraLibrariesMap, importData.ExtraLibraries, &compilerFlags)
	addMacroDefs(ix, toolRepo, importData.Tools, from, &compilerFlags)
	SetAttrIfNotEmpty(r, "compiler_flags", compilerFlags)
}

// Drops from xs all of the macro definitions for the given tools
func dropToolMacroDefs(xs []string, tools []ToolName, ys *[]string) {
	toolMacroDefs := make([]string, 0, len(tools))
	for _, tool := range tools {
		toolMacroDefs = append(toolMacroDefs, "-D"+toolMacroName(tool)+"=")
	}

	for _, x := range xs {
		if !hasAPrefixIn(x, toolMacroDefs) {
			*ys = append(*ys, x)
		}
	}
}

// Adds macros defining the paths for the given tools
func addMacroDefs(
	ix *resolve.RuleIndex,
	toolRepo string,
	tools []ToolName,
	from label.Label,
	ys *[]string,
) {
	for _, tool := range tools {
		toolLabel := findToolLabel(ix, toolRepo, tool, from)
		macroDef := "-D" + toolMacroName(tool) + "=$(location " + toolLabel.String() + ")"
		*ys = append(*ys, macroDef)
	}
}

// Adds unresolved extra libraries as -llib flags.
func addLibraryFlags(
	extraLibrariesMap map[string]string,
	extraLibraries []string,
	ys *[]string,
) {
	for _, lib := range extraLibraries {
		_, ok := extraLibrariesMap[lib]
		if !ok {
			*ys = append(*ys, "-l"+lib)
		}
	}
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

	spec := resolve.ImportSpec{gazelleCabalName, "exe:" + toolString}
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

// All dependencies that are ghc_plugins are placed in the plugins
// attribute. All the dependencies that are indexed comming from
// haskell_library are assumed to be local. There rest of the
// dependencies are assumed to come from packageRepo.
func setDepsAndPluginsAttributes(
	extraLibraries map[string]string,
	packageRepo string,
	ix *resolve.RuleIndex,
	r *rule.Rule,
	importData ImportData,
	from label.Label,
) {
	deps := make([]string, 0, len(importData.Deps)+len(importData.ExtraLibraries))
	plugins := make([]string, 0, len(importData.Deps))
	for _, depName := range importData.Deps {
		plugin, err := getPluginLabel(ix, depName, from)
		if err == nil {
			plugins = append(plugins, plugin.String())
		} else {
			haskellPkg := getPackageLabel(r, ix, packageRepo, depName, from)
			deps = append(deps, haskellPkg.String())
		}
	}
	for _, lib := range importData.ExtraLibraries {
		libLabel, ok := extraLibraries[lib]
		if ok {
			deps = append(deps, libLabel)
		}
	}
	SetAttrIfNotEmpty(r, "deps", deps)
	SetAttrIfNotEmpty(r, "plugins", plugins)
}

// Produces a plugin label if there is a ghc_plugin rule with the
// same name as the package name plus the "-plugin" suffix.
func getPluginLabel(
	ix *resolve.RuleIndex,
	packageName string,
	from label.Label,
) (label.Label, error) {
	spec := resolve.ImportSpec{gazelleCabalName, "ghc_plugin:" + packageName + "-plugin"}
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
	if (packagePrefix == cabalPkgName) {
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
	spec := resolve.ImportSpec{gazelleCabalName, importId}
	res := ix.FindRulesByImport(spec, gazelleCabalName)

	librariesFound := len(res)
	if librariesFound > 1 {
		// There are at least two cabal pkgs with the same name
		log.Fatalf("Multiple labels found under %s for package $s : %s", from, pkgName, res)
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

func SetAttrIfNotEmpty(r *rule.Rule, attr string, values []string) {
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
			packageLabels := r.AttrStrings("deps")
			for _, labelString := range packageLabels {
				label, err := parseAbsoluteLabel(labelString)
				if err == nil && label.Repo == packageRepo && label.Pkg == "" {
					packages[label.Name] = true
				}
			}
			toolLabels := r.AttrStrings("tools")
			for _, labelString := range toolLabels {
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

func toolsToComponents(tools map[string]map[string]bool) map[string][]string {
	toolMaps := make(map[string][]string)
	for toolPackageName, toolExecutableNames := range tools {
		comps := make([]string, len(toolExecutableNames)+1)
		comps[0] = "lib"
		i := 0
		for exeName, _ := range toolExecutableNames {
			comps[i+1] = "exe:" + exeName
			i++
		}
		toolMaps[toolPackageName] = comps
	}
	return toolMaps
}

func mapSortedStringKeys(m map[string]bool) []string {
	ss := make([]string, len(m))
	i := 0
	for s, _ := range m {
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
