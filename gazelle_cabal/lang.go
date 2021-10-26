// An extension for gazelle to generate rules from Cabal files
package gazelle_cabal

import (
	"encoding/json"
	"flag"
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
	return []string{
		"cabal_extra_libraries",
		"cabal_haskell_package_repo",
	}
}

type Config struct {
	ExtraLibrariesMap  map[string]string
	HaskellPackageRepo string
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
			ExtraLibrariesMap:  make(map[string]string),
			HaskellPackageRepo: "stackage",
		}
	}

	for _, directive := range f.Directives {
		switch directive.Key {
		case "cabal_extra_libraries":
			parseExtraLibraries(&extraConfig, directive.Value)
		case "cabal_haskell_package_repo":
			extraConfig.HaskellPackageRepo = directive.Value
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
	MatchAttrs:    []string{},
	NonEmptyAttrs: map[string]bool{},
	ResolveAttrs: map[string]bool{
		"compiler_flags": true,
		"data":           true,
		"deps":           true,
		"plugins":        true,
		"srcs":           true,
		"tools":          true,
		"version":        true,
	},
}

var kinds = map[string]rule.KindInfo{
	"haskell_library": haskellAttrInfo,
	"haskell_binary":  haskellAttrInfo,
	"haskell_test":    haskellAttrInfo,
	"ghc_plugin":      {},
	"stack_snapshot": {
		MergeableAttrs: map[string]bool{
			"packages": true,
			// Disabled since gazelle aborts when merging this
			// type of values.
			// https://github.com/bazelbuild/bazel-gazelle/issues/937
			"components": false,
		},
	},
}

func (*gazelleCabalLang) Kinds() map[string]rule.KindInfo {
	return kinds
}

func (*gazelleCabalLang) Loads() []rule.LoadInfo {
	return []rule.LoadInfo{
		{
			Name:    "@rules_haskell//haskell:defs.bzl",
			Symbols: []string{"haskell_binary", "haskell_library", "haskell_test"},
		},
	}
}

func (*gazelleCabalLang) Imports(c *config.Config, r *rule.Rule, f *rule.File) []resolve.ImportSpec {
	var prefix string
	switch r.Kind() {
	case "ghc_plugin":
		prefix = "ghc_plugin:"
	case "haskell_binary":
		prefix = "exe:"
	case "haskell_library":
		if (r.PrivateAttr("internal_library") != nil) {
			prefix = "internal_library:"
		}
	case "haskell_test":
		prefix = "test:"
	}

	return []resolve.ImportSpec{{gazelleCabalName, prefix + r.Name()}}
}

func (*gazelleCabalLang) Embeds(r *rule.Rule, from label.Label) []label.Label { return nil }

func (*gazelleCabalLang) Resolve(c *config.Config, ix *resolve.RuleIndex, rc *repo.RemoteCache, r *rule.Rule, imports interface{}, from label.Label) {
	extraLibrariesMap := c.Exts[gazelleCabalName].(Config).ExtraLibrariesMap
	packageRepo := c.Exts[gazelleCabalName].(Config).HaskellPackageRepo
	toolRepo := packageRepo + "-exe"
	importData := imports.(ImportData)
	setDepsAndPluginsAttributes(extraLibrariesMap, packageRepo, ix, r, importData, from)
	setCompilerFlagsAttribute(extraLibrariesMap, toolRepo, ix, r, importData, from)
	setToolsAttribute(ix, toolRepo, r, importData, from)
}

func (*gazelleCabalLang) GenerateRules(args language.GenerateArgs) language.GenerateResult {
	// No need to invoke cabalscan if there are no Cabal files here
	cabalFiles := listFilesWithExtension(args.Dir, ".cabal")
	if len(cabalFiles) == 0 {
		return language.GenerateResult{
			Gen:     []*rule.Rule{},
			Imports: []interface{}{},
		}
	}

	ruleInfos := cabalToRuleInfos(cabalFiles)

	generateResult := infoToRules(args.Config.RepoRoot, ruleInfos)

	if (args.File != nil) {
		copyPrivateAttrs(generateResult.Gen, args.File.Rules)
	}

	setVisibilities(args.File, generateResult.Gen)

	return generateResult
}

func (lang *gazelleCabalLang) UpdateRepos(args language.UpdateReposArgs) language.UpdateReposResult {
	packageList, components := collectDependenciesFromRepo(args.Config, lang)

	packageRepo := args.Config.Exts[gazelleCabalName].(Config).HaskellPackageRepo
	r := rule.NewRule("stack_snapshot", packageRepo)
	SetArrayAttr(r, "packages", packageList)
	if len(components) > 0 {
		r.SetAttr("components", components)
	}

	return language.UpdateReposResult{
		Gen: []*rule.Rule{r},
	}
}

func (*gazelleCabalLang) Fix(c *config.Config, f *rule.File) {
	if !c.ShouldFix {
		return
	}
	cabalFiles := listFilesWithExtension(filepath.Dir(f.Path), ".cabal")
	if len(cabalFiles) == 0 || f == nil {
		return
	}

	ruleInfos := cabalToRuleInfos(cabalFiles)

	for _, r := range f.Rules {
		if !r.ShouldKeep() &&
			isHaskellRule(r.Kind()) &&
			!hasRuleInfo(ruleInfos, r.Kind(), r.Name()) {
			r.Delete()
		}
	}
}

func copyPrivateAttrs(from []*rule.Rule, to []*rule.Rule) {
	for _, f := range from {
		for _, t := range to {
			if (f.Name() == t.Name()) {
				for _, key := range f.PrivateAttrKeys() {
					t.SetPrivateAttr(key, f.PrivateAttr(key))
				}
				continue
			}
		}
	}
}

////////////////////////////////////////////////////
// rule generating functions
////////////////////////////////////////////////////

type RuleInfo struct {
	Kind         string
	Name         string
	Attrs        map[string]interface{}
	PrivateAttrs map[string]interface{}
	ImportData   ImportData
	CabalFile    string
}

func infoToRules(repoRoot string, ruleInfos []RuleInfo) language.GenerateResult {
	theRules := make([]*rule.Rule, len(ruleInfos))
	theImports := make([]interface{}, len(ruleInfos))
	for i, ruleInfo := range ruleInfos {
		r := rule.NewRule(ruleInfo.Kind, ruleInfo.Name)
		for k, v := range ruleInfo.Attrs {
			r.SetAttr(k, v)
		}

		for k, v := range ruleInfo.PrivateAttrs {
			r.SetPrivateAttr(k, v)
		}

		file, _ := filepath.Rel(repoRoot, ruleInfo.CabalFile)
		r.AddComment("# rule generated from " + file + " by gazelle_cabal")

		theRules[i] = r
		theImports[i] = ruleInfo.ImportData
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
	if err != nil {
		log.Fatal(err)
	}
	files, err := dirFile.Readdir(-1)
	dirFile.Close()
	if err != nil {
		log.Fatal(err)
	}
	theFiles := make([]string, 0, len(files))
	for _, file := range files {
		if filepath.Ext(file.Name()) == ext {
			theFiles = append(theFiles, path.Join(dir, file.Name()))
		}
	}
	return theFiles
}

const CABAL2BUILD_PATH = "cabalscan/cabalscan"

func cabalToRuleInfos(cabalFiles []string) []RuleInfo {
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

////////////////////////////////////////////////////
// Fix functions
////////////////////////////////////////////////////

func hasRuleInfo(ruleInfos []RuleInfo, kind string, name string) bool {
	for _, info := range ruleInfos {
		if info.Kind == kind && info.Name == name {
			return true
		}
	}
	return false
}
