// An extension for gazelle to generate rules from Cabal files
package gazelle_cabal

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"path"
	"path/filepath"
	"sort"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/repo"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"
	bzl "github.com/bazelbuild/buildtools/build"
	"github.com/bazelbuild/rules_go/go/tools/bazel"

	"os"
	"os/exec"
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
		"cabal_haskell_package_repo",
		// Added to avoid warnings when running :gazelle-update-repos
		// https://github.com/tweag/gazelle_cabal/issues/8
		"resolve",
	}
}

type Config struct {
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
			HaskellPackageRepo: "stackage",
		}
	}

	for _, directive := range f.Directives {
		switch directive.Key {
		case "cabal_haskell_package_repo":
			extraConfig.HaskellPackageRepo = directive.Value
		}
	}
	c.Exts[gazelleCabalName] = extraConfig
}

var haskellAttrInfo = rule.KindInfo{
	MatchAttrs:    []string{},
	NonEmptyAttrs: map[string]bool{},
	ResolveAttrs: map[string]bool{
		"ghcopts":   true,
		"data":      true,
		"deps":      true,
		"main_file": true,
		"plugins":   true,
		"srcs":      true,
		"tools":     true,
		"version":   true,
	},
}

var kinds = map[string]rule.KindInfo{
	"bool_flag":       {},
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
			Name:    "@bazel_skylib//rules:common_settings.bzl",
			Symbols: []string{"bool_flag"},
		},
		{
			Name:    "@bazel_skylib//lib:selects.bzl",
			Symbols: []string{"selects"},
		},
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
		visibility := r.PrivateAttr("visibility")
		pkgName := r.PrivateAttr("pkgName")
		if visibility == "private" {
			prefix = fmt.Sprintf("private_library:%s:", pkgName)
		} else {
			prefix = fmt.Sprintf("public_library:%s:", pkgName)
		}
	case "haskell_test":
		prefix = "test:"
	}

	return []resolve.ImportSpec{{gazelleCabalName, prefix + r.Name()}}
}

func (*gazelleCabalLang) Embeds(r *rule.Rule, from label.Label) []label.Label { return nil }

func (*gazelleCabalLang) Resolve(c *config.Config, ix *resolve.RuleIndex, rc *repo.RemoteCache, r *rule.Rule, imports interface{}, from label.Label) {
	packageRepo := c.Exts[gazelleCabalName].(Config).HaskellPackageRepo
	toolRepo := packageRepo + "-exe"
	if imports != nil {
		importData := imports.(ImportData)

		libraryLabels, unresolvedExtraLibraries := getExtraLibraryLabels(c, importData.ExtraLibraries)
		setDepsAndPluginsAttributes(libraryLabels, packageRepo, ix, r, importData, from)
		setCompilerFlagsAttribute(unresolvedExtraLibraries, toolRepo, ix, r, importData, from)
		setToolsAttribute(ix, toolRepo, r, importData, from)
	}
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

	packageOutput := cabalToPackageOutput(cabalFiles)

	generateResult := infoToRules(args.Config.RepoRoot, packageOutput.Flags, packageOutput.Rules, packageOutput.ConfigSettingGroups)

	if args.File != nil {
		copyPrivateAttrs(generateResult.Gen, args.File.Rules)
	}

	setVisibilities(args.File, generateResult.Gen)

	return generateResult
}

func (lang *gazelleCabalLang) UpdateRepos(args language.UpdateReposArgs) language.UpdateReposResult {
	packageList, components := collectDependenciesFromRepo(args.Config, lang)

	packageRepo := args.Config.Exts[gazelleCabalName].(Config).HaskellPackageRepo
	r := rule.NewRule("stack_snapshot", packageRepo)
	r.SetAttr("packages", packageList)
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

	packageOutput := cabalToPackageOutput(cabalFiles)

	for _, r := range f.Rules {
		if !r.ShouldKeep() &&
			isHaskellRule(r.Kind()) &&
			!hasRuleInfo(packageOutput.Rules, r.Kind(), r.Name()) {
			r.Delete()
		}
	}
}

func copyPrivateAttrs(from []*rule.Rule, to []*rule.Rule) {
	// Convert slice to map to avoid quadratic runtime
	toMap := make(map[string]*rule.Rule)
	for _, r := range to {
		toMap[r.Name()] = r
	}
	for _, rFrom := range from {
		if rTo, ok := toMap[rFrom.Name()]; ok {
			for _, key := range rFrom.PrivateAttrKeys() {
				rTo.SetPrivateAttr(key, rFrom.PrivateAttr(key))
			}
		}
	}
}

type Flag struct {
	Name         string
	DefaultValue bool
}

type PackageOutput struct {
	Flags               []Flag
	Rules               []RuleInfo
	ConfigSettingGroups []ConfigSettingGroup `json:"config_setting_groups"`
}

type ConfigSettingGroup struct {
	Name     string   `json:"name"`
	MatchAll []string `json:"match_all"`
}

////////////////////////////////////////////////////
// rule generating functions
////////////////////////////////////////////////////

type RuleInfo struct {
	Kind          string
	Name          string
	Srcs          ConfigurableList[string]
	HiddenModules ConfigurableList[string] `json:"hidden_modules"`
	Attrs         map[string]interface{}
	PrivateAttrs  map[string]interface{}
	ImportData    ImportData
	CabalFile     string
}

func infoToRules(repoRoot string, flags []Flag, ruleInfos []RuleInfo, configGroups []ConfigSettingGroup) language.GenerateResult {
	numConfigGroups := len(configGroups)
	theRules := make([]*rule.Rule, len(ruleInfos)+2*len(flags)+numConfigGroups)
	theImports := make([]interface{}, len(ruleInfos)+2*len(flags)+numConfigGroups)
	sort.Slice(flags, func(i, j int) bool {
		return flags[i].Name < flags[j].Name
	})
	idx := 0

	// Generate bool_flag and config_setting for each flag
	for _, flag := range flags {
		r := rule.NewRule("bool_flag", flag.Name)
		r.SetAttr("build_setting_default", flag.DefaultValue)
		theRules[idx] = r
		theImports[idx] = nil
		idx++

		r = rule.NewRule("config_setting", "flag_"+flag.Name)
		r.SetAttr("flag_values", map[string]string{
			":" + flag.Name: "True",
		})
		theRules[idx] = r
		theImports[idx] = nil
		idx++
	}

	// Generate config_setting_group for each group
	// Requires: load("@bazel_skylib//lib:selects.bzl", "selects")
	for _, group := range configGroups {
		r := rule.NewRule("selects.config_setting_group", group.Name[1:]) // Remove leading ":"
		r.SetAttr("match_all", group.MatchAll)
		r.AddComment("# AND-chained config settings")
		theRules[idx] = r
		theImports[idx] = nil
		idx++
	}

	// Generate rules for each component
	for _, ruleInfo := range ruleInfos {
		r := rule.NewRule(ruleInfo.Kind, ruleInfo.Name)
		r.SetAttr("srcs", ruleInfo.Srcs)
		if ruleInfo.HiddenModules != nil {
			r.SetAttr("hidden_modules", ruleInfo.HiddenModules)
		}
		for k, v := range ruleInfo.Attrs {
			r.SetAttr(k, v)
		}

		for k, v := range ruleInfo.PrivateAttrs {
			r.SetPrivateAttr(k, v)
		}

		file, _ := filepath.Rel(repoRoot, ruleInfo.CabalFile)
		r.AddComment("# rule generated from " + file + " by gazelle_cabal")

		theRules[idx] = r
		theImports[idx] = ruleInfo.ImportData
		idx++
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

func (c ConfigurableList[T]) BzlExpr() bzl.Expr {
	var expr bzl.Expr
	for _, item := range c {
		bzlExpr := rule.ExprFromValue(item)
		if expr == nil {
			expr = bzlExpr
		} else {
			expr = &bzl.BinaryExpr{X: expr, Y: bzlExpr, Op: "+"}
		}
	}
	if expr == nil {
		return &bzl.ListExpr{}
	}
	return expr
}

func (c ConfigurableList[T]) Merge(other bzl.Expr) bzl.Expr {
	newExpr := c.BzlExpr()

	// If there's no existing expression, just return the new one
	if other == nil {
		return newExpr
	}

	// Collect kept expressions from old expression
	var keptElems []bzl.Expr
	var keptWholeExprs []bzl.Expr
	var keptSelects []*bzl.CallExpr

	walkExprWithContext(other, false, func(expr bzl.Expr, insideSelect bool) {
		if !rule.ShouldKeep(expr) {
			return
		}

		// If the whole select is marked keep, save it as a whole expression
		if selectExpr, ok := expr.(*bzl.CallExpr); ok {
			keptSelects = append(keptSelects, selectExpr)
			keptWholeExprs = append(keptWholeExprs, selectExpr)
			return
		}

		// If we're inside a select and an element is marked keep,
		// we can't easily preserve it - would need to merge select dicts
		// For now, log a warning or keep the whole select
		if insideSelect {
			// Elements inside select branches are complex to merge
			// Skip them for now - user should mark the whole select as keep
			return
		}

		// Check if this is a whole expression (not a list element)
		switch expr.(type) {
		case *bzl.BinaryExpr:
			keptWholeExprs = append(keptWholeExprs, expr)
		default:
			// For simple expressions (strings, etc), these are list elements
			keptElems = append(keptElems, expr)
		}
	})

	// If we have kept elements, we need to merge them into the list
	if len(keptElems) > 0 {
		newExpr = mergeKeptElemsIntoExpr(newExpr, keptElems)
	}

	// Combine with whole expressions that were kept
	result := newExpr
	for _, kept := range keptWholeExprs {
		result = &bzl.BinaryExpr{X: kept, Y: result, Op: "+"}
	}

	return result
}

// walkExprWithContext walks through a bzl.Expr tree with context about whether we're inside a select
func walkExprWithContext(expr bzl.Expr, insideSelect bool, fn func(bzl.Expr, bool)) {
	if expr == nil {
		return
	}

	fn(expr, insideSelect)

	switch e := expr.(type) {
	case *bzl.BinaryExpr:
		if e.Op == "+" {
			walkExprWithContext(e.X, insideSelect, fn)
			walkExprWithContext(e.Y, insideSelect, fn)
		}
	case *bzl.ListExpr:
		for _, elem := range e.List {
			walkExprWithContext(elem, insideSelect, fn)
		}
	case *bzl.CallExpr:
		// Handle select() expressions - mark that we're inside a select
		isSelect := false
		if ident, ok := e.X.(*bzl.Ident); ok && ident.Name == "select" {
			isSelect = true
		}
		for _, arg := range e.List {
			walkExprWithContext(arg, insideSelect || isSelect, fn)
		}
	case *bzl.DictExpr:
		// Inside select's dictionary
		for _, keyVal := range e.List {
			walkExprWithContext(keyVal.Value, insideSelect, fn)
		}
	}
}

// mergeKeptElemsIntoExpr prepends kept elements to the beginning of list expressions
func mergeKeptElemsIntoExpr(expr bzl.Expr, keptElems []bzl.Expr) bzl.Expr {
	switch e := expr.(type) {
	case *bzl.ListExpr:
		// Prepend kept elements to the list
		newList := make([]bzl.Expr, 0, len(keptElems)+len(e.List))
		newList = append(newList, keptElems...)
		newList = append(newList, e.List...)
		return &bzl.ListExpr{List: newList}

	case *bzl.BinaryExpr:
		// If it's a + expression, merge into the leftmost list
		if e.Op == "+" {
			return &bzl.BinaryExpr{
				X:  mergeKeptElemsIntoExpr(e.X, keptElems),
				Y:  e.Y,
				Op: "+",
			}
		}

	case *bzl.CallExpr:
		// For select() expressions, we can't easily merge elements
		// So create a list with kept elements and concatenate
		if len(keptElems) > 0 {
			keptList := &bzl.ListExpr{List: keptElems}
			return &bzl.BinaryExpr{X: keptList, Y: expr, Op: "+"}
		}
	}

	// If we can't merge into the expression, prepend as a list
	if len(keptElems) > 0 {
		keptList := &bzl.ListExpr{List: keptElems}
		return &bzl.BinaryExpr{X: keptList, Y: expr, Op: "+"}
	}

	return expr
}

type ConfigurableList[T any] []ConfigurableListItem
type ConfigurableListItem interface{}

// AddItem adds a single item to a ConfigurableList[T].
// If the list is empty, it creates a new list with this single element.
// If the list is not empty and the last element is a simple Value ([]T),
// it adds the item to that list. Otherwise, it adds a new list with a single element.
func (cl *ConfigurableList[T]) AddItem(item T) {
	if len(*cl) == 0 {
		*cl = append(*cl, []T{item})
		return
	}

	lastIdx := len(*cl) - 1
	if lastValue, ok := (*cl)[lastIdx].([]T); ok {
		(*cl)[lastIdx] = append(lastValue, item)
	} else {
		*cl = append(*cl, []T{item})
	}
}

// Map transforms a ConfigurableList[string] by applying a mapper function to each string element.
// The mapper is applied to each string in both simple value items and select items.
// Returns a new ConfigurableList with the transformed results.
func (cl ConfigurableList[T]) Map(
	mapper func(T) T,
) ConfigurableList[T] {
	return cl.MapFilter(
		func(s T) (T, bool) {
			return mapper(s), true
		},
	)
}

type builtinString = string

// MapFilter transforms a ConfigurableList[string] by applying a mapper function to each string element.
// The mapper returns a string and a boolean indicating whether to include the result.
// Only strings where the mapper returns true are included in the result.
// Returns a new ConfigurableList with the filtered and transformed results.
func (cl ConfigurableList[T]) MapFilter(
	mapper func(T) (T, bool),
) ConfigurableList[T] {
	result := make(ConfigurableList[T], 0, len(cl))
	var zero T
	_, isString := any(zero).(string)

	// Helper to map a slice of T values
	mapSlice := func(values []T) []T {
		mapped := make([]T, 0, len(values))
		for _, val := range values {
			if mappedItem, ok := mapper(val); ok {
				mapped = append(mapped, mappedItem)
			}
		}
		return mapped
	}

	for _, item := range cl {
		switch v := item.(type) {
		case []T:
			if mapped := mapSlice(v); len(mapped) > 0 {
				result = append(result, mapped)
			}

		case map[string][]T:
			if isString {
				log.Fatal("did not expect string value")
			}
			selectResult := make(map[string][]T, len(v))
			for k, values := range v {
				selectResult[k] = mapSlice(values)
			}
			if len(selectResult) > 0 {
				result = append(result, selectResult)
			}

		case rule.SelectStringListValue:
			if !isString {
				log.Fatal("expected SelectStringListValue")
			}
			selectResult := make(rule.SelectStringListValue, len(v))
			for k, values := range v {
				// Since T is string, we can directly cast the slice through any
				tValues := any(values).([]T)
				mappedT := mapSlice(tValues)
				selectResult[k] = any(mappedT).([]string)
			}
			if len(selectResult) > 0 {
				result = append(result, selectResult)
			}

		default:
			log.Fatalf("unexpected ConfigurableList item type: %T", v)
		}
	}
	return result
}

type Value = []string

func unmarshalSelect[T any, Out any](cl *ConfigurableList[T], data []byte) bool {
	var out Out
	if err := json.Unmarshal(data, &out); err == nil {
		*cl = append(*cl, out)
		return true
	}
	return false
}

func (cl *ConfigurableList[T]) UnmarshalJSON(data []byte) error {
	var rawItems []json.RawMessage
	if err := json.Unmarshal(data, &rawItems); err != nil {
		return err
	}
	var zero T
	_, isString := any(zero).(string)

	var tryUnmarshal func(*ConfigurableList[T], []byte) bool
	if isString {
		tryUnmarshal = func(c *ConfigurableList[T], b []byte) bool {
			return unmarshalSelect[T, rule.SelectStringListValue](c, b)
		}
	} else {
		tryUnmarshal = func(c *ConfigurableList[T], b []byte) bool {
			return unmarshalSelect[T, map[string][]T](c, b)
		}
	}
	for _, raw := range rawItems {
		var simpleValue []T
		if err := json.Unmarshal(raw, &simpleValue); err == nil {
			*cl = append(*cl, simpleValue)
			continue
		}

		if !tryUnmarshal(cl, raw) {
			return fmt.Errorf("unknown item type")
		}
	}

	return nil
}

func cabalToPackageOutput(cabalFiles []string) PackageOutput {
	gazelleCabalPath, err := bazel.Runfile(CABAL2BUILD_PATH)
	if err != nil {
		log.Fatal(err)
	}
	cmd := exec.Command(gazelleCabalPath, cabalFiles...)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err = cmd.Run()
	if err != nil {
		log.Printf("%s", stderr.String())
		log.Fatal(err)
	}
	var packageOutput PackageOutput
	err = json.Unmarshal(stdout.Bytes(), &packageOutput)
	if err != nil {
		log.Printf("Incorrect json: %s\n", stdout.String())
		log.Fatal(err)
	}
	return packageOutput
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
