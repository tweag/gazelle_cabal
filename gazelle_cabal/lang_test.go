package gazelle_cabal

import (
	"github.com/bazelbuild/bazel-gazelle/rule"
	"reflect"
	"testing"
)

func TestCopyPrivateAttrs(t *testing.T) {
	wanted := map[string]int{
		"a": 0, "b": 1, "c": 2, "d": 3,
	}

	from := rule.NewRule("from", "tweag")
	to := rule.NewRule("from", "tweag")

	for k, v := range wanted {
		from.SetPrivateAttr(k, v)
	}

	copyPrivateAttrs([]*rule.Rule{from}, []*rule.Rule{to})

	got := make(map[string]int)
	for _, key := range to.PrivateAttrKeys() {
		got[key] = to.PrivateAttr(key).(int)
	}

	if !reflect.DeepEqual(got, wanted) {
		t.Errorf("got %v, wanted %v", got, wanted)
	}
}

func TestConsolidateConfigurableList(t *testing.T) {
	tests := []struct {
		name     string
		input    ConfigurableList[string]
		expected ConfigurableList[string]
	}{
		{
			name:     "empty list",
			input:    ConfigurableList[string]{},
			expected: ConfigurableList[string]{},
		},
		{
			name: "plain values only",
			input: ConfigurableList[string]{
				[]string{"a", "b", "c"},
				[]string{"b", "c", "d"},
			},
			expected: ConfigurableList[string]{
				[]string{"a", "b", "c", "d"},
			},
		},
		{
			name: "remove duplicates from plain values",
			input: ConfigurableList[string]{
				[]string{"a", "b", "a"},
			},
			expected: ConfigurableList[string]{
				[]string{"a", "b"},
			},
		},
		{
			name: "single select",
			input: ConfigurableList[string]{
				rule.SelectStringListValue{
					":flag_x": []string{"a", "b"},
					"//conditions:default": []string{},
				},
			},
			expected: ConfigurableList[string]{
				rule.SelectStringListValue{
					":flag_x": []string{"a", "b"},
					"//conditions:default": []string{},
				},
			},
		},
		{
			name: "merge identical conditions",
			input: ConfigurableList[string]{
				rule.SelectStringListValue{
					":flag_x": []string{"a"},
					"//conditions:default": []string{},
				},
				rule.SelectStringListValue{
					":flag_x": []string{"b"},
					"//conditions:default": []string{},
				},
			},
			expected: ConfigurableList[string]{
				rule.SelectStringListValue{
					":flag_x": []string{"a", "b"},
					"//conditions:default": []string{},
				},
			},
		},
		{
			name: "deduplicate within conditions",
			input: ConfigurableList[string]{
				rule.SelectStringListValue{
					":flag_x": []string{"a", "b"},
					"//conditions:default": []string{},
				},
				rule.SelectStringListValue{
					":flag_x": []string{"a", "c"},
					"//conditions:default": []string{},
				},
			},
			expected: ConfigurableList[string]{
				rule.SelectStringListValue{
					":flag_x": []string{"a", "b", "c"},
					"//conditions:default": []string{},
				},
			},
		},
		{
			name: "remove plain values from selects",
			input: ConfigurableList[string]{
				[]string{"a", "b"},
				rule.SelectStringListValue{
					":flag_x": []string{"a", "c"},
					"//conditions:default": []string{"b"},
				},
			},
			expected: ConfigurableList[string]{
				[]string{"a", "b"},
				rule.SelectStringListValue{
					":flag_x": []string{"c"},
					"//conditions:default": []string{},
				},
			},
		},
		{
			name: "split selects by shared values",
			input: ConfigurableList[string]{
				rule.SelectStringListValue{
					":flag_all": []string{"a", "b"},
					":flag_a": []string{"a"},
					":flag_b": []string{"b"},
					"//conditions:default": []string{},
				},
			},
			expected: ConfigurableList[string]{
				rule.SelectStringListValue{
					":flag_all": []string{"a"},
					":flag_a": []string{"a"},
					"//conditions:default": []string{},
				},
				rule.SelectStringListValue{
					":flag_all": []string{"b"},
					":flag_b": []string{"b"},
					"//conditions:default": []string{},
				},
			},
		},
		{
			name: "complex example - multiple overlapping conditions",
			input: ConfigurableList[string]{
				rule.SelectStringListValue{
					":flag_all_extensions": []string{"@stackage//:dbus"},
					":flag_with_dbus": []string{"@stackage//:dbus"},
					"//conditions:default": []string{},
				},
				rule.SelectStringListValue{
					":flag_all_extensions": []string{"@stackage//:dbus"},
					":flag_with_mpris": []string{"@stackage//:dbus"},
					"//conditions:default": []string{},
				},
			},
			expected: ConfigurableList[string]{
				rule.SelectStringListValue{
					":flag_all_extensions": []string{"@stackage//:dbus"},
					":flag_with_dbus": []string{"@stackage//:dbus"},
					"//conditions:default": []string{},
				},
				rule.SelectStringListValue{
					":flag_all_extensions": []string{"@stackage//:dbus"},
					":flag_with_mpris": []string{"@stackage//:dbus"},
					"//conditions:default": []string{},
				},
			},
		},
		{
			name: "split by different condition sets",
			input: ConfigurableList[string]{
				rule.SelectStringListValue{
					":flag_all_extensions": []string{"-DALSA", "-DDBUS", "-DINOTIFY"},
					":flag_with_alsa": []string{"-DALSA"},
					":flag_with_dbus": []string{"-DDBUS"},
					":flag_with_inotify": []string{"-DINOTIFY"},
					"//conditions:default": []string{},
				},
			},
			expected: ConfigurableList[string]{
				rule.SelectStringListValue{
					":flag_all_extensions": []string{"-DALSA"},
					":flag_with_alsa": []string{"-DALSA"},
					"//conditions:default": []string{},
				},
				rule.SelectStringListValue{
					":flag_all_extensions": []string{"-DDBUS"},
					":flag_with_dbus": []string{"-DDBUS"},
					"//conditions:default": []string{},
				},
				rule.SelectStringListValue{
					":flag_all_extensions": []string{"-DINOTIFY"},
					":flag_with_inotify": []string{"-DINOTIFY"},
					"//conditions:default": []string{},
				},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := consolidateConfigurableList(tt.input)
			
			if !configurableListsEqual(result, tt.expected) {
				t.Errorf("consolidateConfigurableList() =\n%v\nwant:\n%v", result, tt.expected)
			}
		})
	}
}

// Helper function to compare ConfigurableLists
func configurableListsEqual(a, b ConfigurableList[string]) bool {
	if len(a) != len(b) {
		return false
	}
	
	// For each item in a, find a matching item in b
	matchedB := make(map[int]bool)
	
	for _, aItem := range a {
		found := false
		for bIdx, bItem := range b {
			if matchedB[bIdx] {
				continue
			}
			
			if configurableItemsEqual(aItem, bItem) {
				matchedB[bIdx] = true
				found = true
				break
			}
		}
		if !found {
			return false
		}
	}
	
	return true
}

func configurableItemsEqual(a, b interface{}) bool {
	switch aVal := a.(type) {
	case []string:
		bVal, ok := b.([]string)
		if !ok {
			return false
		}
		return stringSlicesEqual(aVal, bVal)
		
	case rule.SelectStringListValue:
		bVal, ok := b.(rule.SelectStringListValue)
		if !ok {
			return false
		}
		return selectsEqual(aVal, bVal)
		
	case map[string][]string:
		// Convert to SelectStringListValue for comparison
		aSelect := make(rule.SelectStringListValue)
		for k, v := range aVal {
			aSelect[k] = v
		}
		
		switch bVal := b.(type) {
		case map[string][]string:
			bSelect := make(rule.SelectStringListValue)
			for k, v := range bVal {
				bSelect[k] = v
			}
			return selectsEqual(aSelect, bSelect)
		case rule.SelectStringListValue:
			return selectsEqual(aSelect, bVal)
		}
	}
	
	return false
}

func stringSlicesEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func selectsEqual(a, b rule.SelectStringListValue) bool {
	if len(a) != len(b) {
		return false
	}
	
	for key, aVals := range a {
		bVals, exists := b[key]
		if !exists {
			return false
		}
		if !stringSlicesEqual(aVals, bVals) {
			return false
		}
	}
	
	return true
}

