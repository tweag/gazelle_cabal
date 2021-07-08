// An extension for gazelle to generate rules from cabal files
package gazelle_cabal

import (
    "flag"
    "encoding/json"
    "fmt"
    "log"
    "path"
    "path/filepath"

    "github.com/bazelbuild/bazel-gazelle/config"
    "github.com/bazelbuild/bazel-gazelle/label"
    "github.com/bazelbuild/bazel-gazelle/language"
    "github.com/bazelbuild/bazel-gazelle/repo"
    "github.com/bazelbuild/bazel-gazelle/resolve"
    "github.com/bazelbuild/bazel-gazelle/rule"
    "github.com/bazelbuild/rules_go/go/tools/bazel"

    "os"
    "os/exec"
    "strings"
)

////////////////////////////////////////////////////
// gazelle callbacks
////////////////////////////////////////////////////

const gazelleCabalName = "gazelle_cabal"

type gazelleCabalLang struct{}

func NewLanguage() language.Language {
    return &gazelleCabalLang{}
}

func (*gazelleCabalLang) Name() string { return gazelleCabalName }

func (*gazelleCabalLang) RegisterFlags(fs *flag.FlagSet, cmd string, c *config.Config) {}

func (*gazelleCabalLang) CheckFlags(fs *flag.FlagSet, c *config.Config) error { return nil }

func (*gazelleCabalLang) KnownDirectives() []string {
    return []string{"cabal_extra_libraries"}
}

type Config struct {
    ExtraLibrariesMap map[string]string
}

func (*gazelleCabalLang) Configure(c *config.Config, rel string, f *rule.File) {
    if f == nil {
        return
    }

    m, ok := c.Exts[gazelleCabalName]
    var extraConfig Config
    if ok {
        extraConfig = m.(Config)
    } else {
        extraConfig = Config{
            ExtraLibrariesMap: make(map[string]string),
        }
    }

    for _, directive := range f.Directives {
        switch directive.Key {
        case "cabal_extra_libraries":
            parseExtraLibraries(&extraConfig, directive.Value)
        }
    }
    c.Exts[gazelleCabalName] = extraConfig
}

func parseExtraLibraries(config *Config, value string) {
    kv := strings.Split(value, "=")
    if len(kv) != 2 {
        msg := "Can't parse value of cabal_extra_libraries: %s"
        err := fmt.Errorf(msg, value)
        log.Fatal(err)
    }
    (*config).ExtraLibrariesMap[kv[0]] = kv[1]
}

var haskellAttrInfo = rule.KindInfo{
    MergeableAttrs: map[string]bool{
        "compiler_flags": true,
        "data": true,
        "deps": true,
        "plugins": true,
        "srcs": true,
        "tools": true,
        "version": true,
    },
}

var kinds = map[string]rule.KindInfo{
    "haskell_library": haskellAttrInfo,
    "haskell_binary": haskellAttrInfo,
    "haskell_test": haskellAttrInfo,
    "ghc_plugin": rule.KindInfo{},
    "stack_snapshot": rule.KindInfo{
        MergeableAttrs: map[string]bool{
            "packages": true,
            // Disabled since gazelle crashes when merging this
            // type of values
            "components": false,
        },
    },
    "stack_snapshot_components": rule.KindInfo{
        MergeableAttrs: map[string]bool{
            "components": true,
        },
    },
}

func (*gazelleCabalLang) Kinds() map[string]rule.KindInfo {
    return kinds
}

func (*gazelleCabalLang) Loads() []rule.LoadInfo {
    return []rule.LoadInfo{
        rule.LoadInfo{
            Name: "@rules_haskell//haskell:defs.bzl",
            Symbols: []string{"haskell_binary", "haskell_library", "haskell_test"},
        },
        rule.LoadInfo{
            Name: "@io_tweag_gazelle_cabal//:stack_snapshot_components/setup.bzl",
            Symbols: []string{"stack_snapshot_components"},
        },
        rule.LoadInfo{
            Name: "@stack_snapshot_components//:components.bzl",
            Symbols: []string{"COMPONENTS"},
        },
    }
}

func (*gazelleCabalLang) Fix(c *config.Config, f *rule.File) {}

func (*gazelleCabalLang) Imports(c *config.Config, r *rule.Rule, f *rule.File) []resolve.ImportSpec {
    var prefix string
    switch r.Kind() {
    case "ghc_plugin":
        prefix = "ghc_plugin:"
    case "haskell_binary":
        prefix = "exe:"
    case "haskell_test":
        prefix = "test:"
    }
    return []resolve.ImportSpec{ resolve.ImportSpec{gazelleCabalName, prefix + r.Name()} }
}

func (*gazelleCabalLang) Embeds(r *rule.Rule, from label.Label) []label.Label { return nil }

func (*gazelleCabalLang) Resolve(c *config.Config, ix *resolve.RuleIndex, rc *repo.RemoteCache, r *rule.Rule, imports interface{}, from label.Label) {
    extraLibrariesMap := c.Exts[gazelleCabalName].(Config).ExtraLibrariesMap
    importData := imports.(ImportData)
    setDepsAndPluginsAttributes(extraLibrariesMap, ix, r, importData)
    setCompilerFlagsAttribute(extraLibrariesMap, ix, r, importData)
    setToolsAttribute(ix, r, importData)
}

func (*gazelleCabalLang) GenerateRules(args language.GenerateArgs) language.GenerateResult {
    // No need to invoke cabalscan if there are no .cabal files here
    cabalFiles := listFilesWithExtension(args.Dir, ".cabal")
    if len(cabalFiles) == 0 {
        return language.GenerateResult{
            Gen:     []*rule.Rule{},
            Imports: []interface{}{},
        }
    }

    ruleInfos := cabalToRuleInfos(args.Config.RepoRoot, cabalFiles)

    generateResult := infoToRules(ruleInfos)

    setVisibilities(args.File, generateResult.Gen)

    return generateResult
}

func (lang *gazelleCabalLang) UpdateRepos(args language.UpdateReposArgs) language.UpdateReposResult {
    packageList, components := collectDependenciesFromRepo(args.Config, lang)

    r1 := rule.NewRule("stack_snapshot_components", "stack_snapshot_components")
    r1.SetAttr("components", componentsToStrings(components))

    r2 := rule.NewRule("stack_snapshot", "stackage")
    r2.SetAttr("packages", packageList)
    if len(components) > 0 {
        r2.SetAttr("components", components)
    }

    if len(components) > 0 {
        return language.UpdateReposResult{
            Gen:     []*rule.Rule{r1, r2},
        }
    } else {
        return language.UpdateReposResult{
            Gen:     []*rule.Rule{r2},
        }
    }
}

////////////////////////////////////////////////////
// rule generating functions
////////////////////////////////////////////////////

type RuleInfo struct {
    Kind string
    Name string
    Attrs map[string]interface{}
    ImportData ImportData
}

func infoToRules(ruleInfos []RuleInfo) language.GenerateResult {

    theRules := make([]*rule.Rule, 0, len(ruleInfos))
    theImports := make([]interface{}, 0, len(ruleInfos))
    for _, ruleInfo := range ruleInfos {
        r := rule.NewRule(ruleInfo.Kind, ruleInfo.Name)
        for k, v := range ruleInfo.Attrs {
          r.SetAttr(k, v)
        }
        theRules = append(theRules, r)
        theImports = append(theImports, ruleInfo.ImportData)
    }

    return language.GenerateResult{
        Gen:     theRules,
        Imports: theImports,
    }
}

func setVisibilities(f *rule.File, rules []*rule.Rule) {
    if f == nil || !f.HasDefaultVisibility() {
        for _, r := range rules {
            r.SetAttr("visibility", []string{"//visibility:public"})
        }
    }
}

// Lists files in the given directory with the given extension
func listFilesWithExtension(dir string, ext string) []string {
    dirFile, err := os.Open(dir)
    if err != nil { log.Fatal(err) }
    files, err := dirFile.Readdir(-1)
    dirFile.Close()
    if err != nil { log.Fatal(err) }
    theFiles := make([]string, 0, len(files))
    for _, file := range files {
        if filepath.Ext(file.Name()) == ext {
            theFiles = append(theFiles, path.Join(dir, file.Name()))
        }
    }
    return theFiles
}

const CABAL2BUILD_PATH = "cabalscan/cabalscan"

func cabalToRuleInfos(repoRoot string, cabalFiles []string) []RuleInfo {
    gazelleCabalPath, err := bazel.Runfile(CABAL2BUILD_PATH)
    if err != nil {
        log.Fatal(err)
    }
    cmd := exec.Command(gazelleCabalPath, cabalFiles...)
    out, err := cmd.CombinedOutput()
    if err != nil {
        log.Printf("%s", out)
        log.Fatal(err)
    }
    var ruleInfos []RuleInfo
    err = json.Unmarshal(out, &ruleInfos)
    if err != nil {
        log.Printf("Incorrect json: %s\n", out)
        log.Fatal(err)
    }
    return ruleInfos
}
