package gazelle_cabal

import (
	"reflect"
	"testing"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

func TestCopyPrivateAttrs(t *testing.T) {
	wanted := map[string]int{
		"a": 0, "b": 1, "c": 2, "d": 3,
	}

	from := rule.NewRule("from", "tweag")
	to := rule.NewRule("from", "tweag")

	for k,v := range wanted {
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
