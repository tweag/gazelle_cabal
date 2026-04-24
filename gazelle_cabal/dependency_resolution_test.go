package gazelle_cabal

import (
	"reflect"
	"testing"
)

func TestDropToolMacroDefs_OnlyRelevantMacrosAreDropped(t *testing.T) {
	flags := ConfigurableList[string]{[]string{
		"-DTHE_PKG_THE_EXE_PATH=/pkg/exe",
		"-f",
		"-DTHE_PKG=/another",
		"-DTHE_PKG_THE_EXE_PATH=/pkg/exe2/exe",
		"-DTHE_PKG_THE_EXE_PATH=",
	}}
	tools := ConfigurableList[ToolName]{[]ToolName{{"the-pkg", "the-exe"}}}
	wanted := []string{
		"-f",
		"-DTHE_PKG=/another",
	}
	got := dropToolMacroDefs(flags, tools)
	if len(got) != 1 {
		t.Fatalf("expected 1 item in got, but got %d", len(got))
	}
	result, ok := got[0].([]string)
	if !ok {
		t.Fatalf("expected []string, got %T", got[0])
	}
	if !reflect.DeepEqual(result, wanted) {
		t.Errorf("got %v, wanted %v", result, wanted)
	}
}

func TestDropToolMacroDefs_KeepsEverythingIfNoTools(t *testing.T) {
	wanted := []string{
		"-DTHE_PKG_THE_EXE_PATH=/pkg/exe",
		"-f",
		"-DTHE_PKG=/another",
		"-DTHE_PKG_THE_EXE_PATH=/pkg/exe2/exe",
		"-DTHE_PKG_THE_EXE_PATH=",
	}
	flags := ConfigurableList[string]{wanted}
	tools := ConfigurableList[ToolName]{[]ToolName{}}
	got := dropToolMacroDefs(flags, tools)
	if len(got) != 1 {
		t.Fatalf("expected 1 item in got, but got %d", len(got))
	}
	result, ok := got[0].([]string)
	if !ok {
		t.Fatalf("expected []string, got %T", got[0])
	}
	if !reflect.DeepEqual(result, wanted) {
		t.Errorf("got %v, wanted %v", result, wanted)
	}
}

func TestDropToolMacroDefs_DropsMacrosForMultipleToos(t *testing.T) {
	flags := ConfigurableList[string]{[]string{
		"-DTHE_PKG_THE_EXE_PATH=/pkg/exe",
		"-f",
		"-DTHE_PKG=/another",
		"-DPKG2_EXE2_PATH=",
		"-DTHE_PKG_THE_EXE_PATH=/pkg/exe2/exe",
		"-DTHE_PKG_THE_EXE_PATH=",
	}}
	tools := ConfigurableList[ToolName]{[]ToolName{
		{"the-pkg", "the-exe"},
		{"pkg2", "exe2"},
	}}
	wanted := []string{
		"-f",
		"-DTHE_PKG=/another",
	}
	got := dropToolMacroDefs(flags, tools)
	if len(got) != 1 {
		t.Fatalf("expected 1 item in got, but got %d", len(got))
	}
	result, ok := got[0].([]string)
	if !ok {
		t.Fatalf("expected []string, got %T", got[0])
	}
	if !reflect.DeepEqual(result, wanted) {
		t.Errorf("got %v, wanted %v", result, wanted)
	}
}

func TestToolMacroName(t *testing.T) {

	got := toolMacroName(ToolName{"the-pkg", "the-exe"})
	wanted := "THE_PKG_THE_EXE_PATH"
	if got != wanted {
		t.Errorf("got %v, wanted %v", got, wanted)
	}

	got = toolMacroName(ToolName{"pkg2", "exe2"})
	wanted = "PKG2_EXE2_PATH"
	if got != wanted {
		t.Errorf("got %v, wanted %v", got, wanted)
	}
}

func TestAddLibraryFlags(t *testing.T) {

	libraries := ConfigurableList[string]{[]string{"m", "SDL"}}
	got := addLibraryFlags(libraries)
	wanted := []string{"-lm", "-lSDL"}
	if len(got) != 1 {
		t.Fatalf("expected 1 items in got, but got %d (%v)", len(got), got)
	}
	result := []string{}
	for _, item := range got {
		if s, ok := item.([]string); ok {
			result = append(result, s...)
		}
	}
	if !reflect.DeepEqual(result, wanted) {
		t.Errorf("got %v, wanted %v", result, wanted)
	}

	libraries = ConfigurableList[string]{[]string{}}
	got = addLibraryFlags(libraries)
	if len(got) != 0 {
		t.Errorf("expected 0 items in got, but got %d", len(got))
	}
}

func TestHasAPrefixIn(t *testing.T) {

	got := hasAPrefixIn("abc", []string{"abcd", "def"})
	wanted := false
	if got != wanted {
		t.Errorf("got %v, wanted %v", got, wanted)
	}

	got = hasAPrefixIn("abcdefg", []string{"abc", "def"})
	wanted = true
	if got != wanted {
		t.Errorf("got %v, wanted %v", got, wanted)
	}

	got = hasAPrefixIn("abcdefg", []string{"abc0", "def"})
	wanted = false
	if got != wanted {
		t.Errorf("got %v, wanted %v", got, wanted)
	}

	got = hasAPrefixIn("abcdefg", []string{"abc0", "def"})
	wanted = false
	if got != wanted {
		t.Errorf("got %v, wanted %v", got, wanted)
	}

	got = hasAPrefixIn("abc", []string{"abcdef"})
	wanted = false
	if got != wanted {
		t.Errorf("got %v, wanted %v", got, wanted)
	}
}

func TestMapSortedStringKeys(t *testing.T) {
	m := map[string]bool{"a": true, "b": false}
	got := mapSortedStringKeys(m)
	wanted := []string{"a", "b"}
	if !reflect.DeepEqual(got, wanted) {
		t.Errorf("got %v, wanted %v", got, wanted)
	}

	m = map[string]bool{}
	got = mapSortedStringKeys(m)
	wanted = []string{}
	if !reflect.DeepEqual(got, wanted) {
		t.Errorf("got %v, wanted %v", got, wanted)
	}
}

func TestParseStackageExeLabel(t *testing.T) {

	s := "@stackage-exe//tasty-discover"
	got, err := parseAbsoluteLabel(s)
	wanted := s
	if err != nil {
		t.Errorf("got %v, wanted %v", err, nil)
	}
	if !reflect.DeepEqual(got.String(), wanted) {
		t.Errorf("got %v, wanted %v", got, wanted)
	}

	s = "@stackage-exe//tasty-discover:tasty"
	got, err = parseAbsoluteLabel(s)
	wanted = s
	if err != nil {
		t.Errorf("got %v, wanted %v", err, nil)
	}
	if !reflect.DeepEqual(got.String(), wanted) {
		t.Errorf("got %v, wanted %v", got, wanted)
	}

	s = "@stackage-exe//tasty-discover:tasty-discover"
	got, err = parseAbsoluteLabel(s)
	wanted = "@stackage-exe//tasty-discover"
	if err != nil {
		t.Errorf("got %v, wanted %v", err, nil)
	}
	if !reflect.DeepEqual(got.String(), wanted) {
		t.Errorf("got %v, wanted %v", got, wanted)
	}

	s = "@stackage-exe//tasty-discover:tasty"
	got, err = parseAbsoluteLabel(s)
	wanted = "(stackage-exe, tasty-discover, tasty)"
	if err != nil {
		t.Errorf("got %v, wanted %v", err, nil)
	}
	if got.Repo != "stackage-exe" || got.Pkg != "tasty-discover" || got.Name != "tasty" {
		t.Errorf("got (%s, %s, %s) wanted %s", got.Repo, got.Pkg, got.Name, wanted)
	}
}

func TestToolsToComponents(t *testing.T) {

	tools := map[string]map[string]bool{
		"tasty-discover": {"tasty-discover": false, "tasty": true},
		"hsinspect":      {"hsinspect": false},
	}
	got := toolsToComponents(tools)
	wanted := map[string][]string{
		"tasty-discover": {"lib", "exe:tasty", "exe:tasty-discover"},
		"hsinspect":      {"lib", "exe:hsinspect"},
	}
	if !reflect.DeepEqual(got, wanted) {
		t.Errorf("got %v, wanted %v", got, wanted)
	}
}
