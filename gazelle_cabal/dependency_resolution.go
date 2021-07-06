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
    Deps []string
    CompilerFlags []string
    ExtraLibraries []string
    Tools []ToolName
}

type ToolName struct {
    PackageName string
    ExecutableName string
}

func setToolsAttribute(ix *resolve.RuleIndex, r *rule.Rule, importData ImportData) {
    tools := make([]string, 0, len(importData.Tools))
    for _, tool := range importData.Tools {
        toolLabel := findToolLabel(ix, tool)
        tools = append(tools, toolLabel)
    }

    r.SetAttr("tools", tools)
}

// Drops preexisting macro definitions for tools. Adds new macro
// definitions for tools. Adds unresolved extra libraries as -llib
// flags.
func setCompilerFlagsAttribute(
        extraLibrariesMap map[string]string,
        ix *resolve.RuleIndex,
        r *rule.Rule,
        importData ImportData,
) {
    compilerFlagsSize :=
        len(importData.ExtraLibraries) +
        len(importData.CompilerFlags) +
        len(importData.Tools)
    compilerFlags := make([]string, 0, compilerFlagsSize)

    dropMacroDefs(importData.CompilerFlags, importData.Tools, &compilerFlags)
    addLibraryFlags(extraLibrariesMap, importData.ExtraLibraries, &compilerFlags)
    addMacroDefs(ix, importData.Tools, &compilerFlags)
    r.SetAttr("compiler_flags", compilerFlags)
}

// Drops from xs all of the macro definitions for the given tools
func dropMacroDefs(xs []string, tools []ToolName, ys *[]string) {
    toolMacroDefs := make([]string, 0, len(tools))
    for _, tool := range tools {
        toolMacroDefs = append(toolMacroDefs, "-D" + toolMacroName(tool) + "=")
    }

    for _, x := range xs {
        if !hasAPrefixIn(x, toolMacroDefs) {
            *ys = append(*ys, x)
        }
    }
}

// Adds macros defining the paths for the given tools
func addMacroDefs(ix *resolve.RuleIndex, tools []ToolName, ys *[]string) {
    for _, tool := range tools {
        toolLabel := findToolLabel(ix, tool)
        macroDef := "-D" + toolMacroName(tool) + "=$(location " + toolLabel + ")"
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
            *ys = append(*ys, "-l" + lib)
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

func findToolLabel(ix *resolve.RuleIndex, tool ToolName) string {
    toolString := tool.ExecutableName
    if tool.PackageName == tool.ExecutableName {
        toolString = toolString + "-binary"
    }

    spec := resolve.ImportSpec{gazelleCabalName, "exe:" + toolString}
    res := ix.FindRulesByImport(spec, gazelleCabalName)
    if len(res) > 0 {
        return res[0].Label.String()
    } else {
        return "@stackage-exe//" + tool.PackageName + ":" + tool.ExecutableName
    }
}

// All dependencies that are ghc_plugins are placed in the plugins
// attribute. All the dependencies that are indexed comming from
// haskell_library are assumed to be local. There rest of the
// dependencies are assumed to come from @stackage.
func setDepsAndPluginsAttributes(
        extraLibraries map[string]string,
        ix *resolve.RuleIndex,
        r *rule.Rule,
        importData ImportData,
) {
    deps := make([]string, 0, len(importData.Deps) + len(importData.ExtraLibraries))
    plugins := make([]string, 0, len(importData.Deps))
    for _, depName := range importData.Deps {
        plugin, err := getPluginLabel(ix, depName)
        if err == nil {
            plugins = append(plugins, plugin)
        } else {
            deps = append(deps, getPackageLabel(ix, depName))
        }
    }
    for _, lib := range importData.ExtraLibraries {
        libLabel, ok := extraLibraries[lib]
        if ok {
            deps = append(deps, libLabel)
        }
    }
    r.SetAttr("deps", deps)
    r.SetAttr("plugins", plugins)
}

// Produces a plugin label if there is a ghc_plugin rule with the
// same name as the package name plus the "-plugin" suffix.
func getPluginLabel(ix *resolve.RuleIndex, packageName string) (string, error) {
    spec := resolve.ImportSpec{gazelleCabalName, "ghc_plugin:" + packageName + "-plugin"}
    res := ix.FindRulesByImport(spec, gazelleCabalName)
    if len(res) > 0 {
        return res[0].Label.String(), nil
    } else {
        return "", errors.New("Not a plugin: " + packageName)
    }
}

// Produces a label for the given package. We assume that if no rule
// is indexed with the package name, the package must come from stackage.
func getPackageLabel(ix *resolve.RuleIndex, packageName string) string {
    spec := resolve.ImportSpec{gazelleCabalName, packageName}
    res := ix.FindRulesByImport(spec, gazelleCabalName)
    if len(res) > 0 {
        return res[0].Label.String()
    } else {
        return "@stackage//:" + packageName
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
            collectDependenciesFromFile(f, packages, tools)
        },
    )
    packageList := mapSortedStringKeys(packages)
    components := toolsToComponents(tools)
    return packageList, components
}

func collectDependenciesFromFile(
    f *rule.File,
    packages map[string]bool,
    tools map[string]map[string]bool,
) {
    if f == nil {
        return
    }
    for _, r := range f.Rules {
        if isHaskellRule(r.Kind()) {
            packageLabels := r.AttrStrings("deps")
            for _, labelString := range packageLabels {
                label, err := label.Parse(labelString)
                if err == nil && label.Repo == "stackage" && label.Pkg == "" {
                    packages[label.Name] = true
                }
            }
            toolLabels := r.AttrStrings("tools")
            for _, labelString := range toolLabels {
                // label.Parse chokes on "@stackage-exe//" with
                // label parse error: repository has invalid characters
                label, err := parseStackageExeLabel(labelString)
                if err == nil {
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
        comps := make([]string, len(toolExecutableNames) + 1)
        comps[0] = "lib"
        i := 0
        for exeName, _ := range toolExecutableNames {
            comps[i + 1] = "exe:" + exeName
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

func parseStackageExeLabel(v string) (label.Label, error) {
    if strings.HasPrefix(v, "@stackage-exe//") {
        repo := strings.Split(v, "//")
        if len(repo) > 1 {
            pkg := strings.Split(repo[1], ":")
            if len(pkg) > 1 {
                return label.New("stackage-exe", pkg[0], pkg[1]), nil
            } else {
                return label.New("stackage-exe", pkg[0], pkg[0]), nil
            }
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
