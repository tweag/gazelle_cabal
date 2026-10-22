set -euo pipefail

# Quick exit if GHC version doesn't conform to expected structure.
if [[ $# -ne 1 || ! $1 =~ ^ghc_[0-9]+_[0-9]+_[0-9]+$ ]]; then
  echo "Usage: $0 ghc_X_Y_Z" >&2
  exit 1
fi

GHC_VERSION=$1

echo "GHC_VERSION is $GHC_VERSION"

nix-shell --pure --run "bazel run //:gazelle --config=$GHC_VERSION"
# Gazelle doesn't remove rules by default
grep -q another-haskell-binary package-a/BUILD.bazel
nix-shell --pure --run "bazel run //:gazelle --config=$GHC_VERSION -- fix -mode diff" || {
  status=$?
  if [[ $status -ne 1 ]]; then
    exit "$status"
  fi
}

set -x
nix-shell --pure --run "bazel run //:gazelle --config=$GHC_VERSION -- fix"
# Test that fix kept and removed the expected rules
! grep -q another-haskell-binary package-a/BUILD.bazel
grep -q a-haskell-binary package-a/BUILD.bazel
grep -q haskell_toolchain_library package-a/BUILD.bazel
# Test main_file attribute (taken from cabal's main_is)
# Simple scenario: main-is: Main.hs & hs-source-dirs: app
grep -q "main_file = \"app/Main.hs\"" package-a/BUILD.bazel
# Check concatenation when main-is: prank/Main.B and hs-source-dirs: app
grep -q "main_file = \"app/prank/MainB.hs\"" package-a/BUILD.bazel
# Test sublibrary generation
grep -q sublibPub package-b/BUILD.bazel
grep -q mtl package-b/BUILD.bazel
# Test sublibrary shadowing
grep -q :sublibPub package-b/BUILD.bazel
grep -q :mtl package-b/BUILD.bazel
# Test public internal library feature
grep -q //package-b:sublibPub package-a/BUILD.bazel
# Test dependency on local libraries (the same name)
grep -q :sublibPub package-a/BUILD.bazel
grep -q :sublibPriv package-a/BUILD.bazel
# Test dependency on local libraries (the same name)
# with experimental colon syntax
grep -q :colonPub package-a/BUILD.bazel
grep -q :colonPriv package-a/BUILD.bazel
# Test existence of package-b main lib dependency
# referenced with colon syntax: package-b:package-b
grep -q //package-b package-a/BUILD.bazel
# Test existence of package-c main library
# referenced without colon syntax: package-c
grep -q //package-c package-a/BUILD.bazel
# Test that unknown libraries referenced with colon syntax
# are resolved from repository
grep -q @stackage//:tasty package-a/BUILD.bazel
# Test sublibrary local dependency resolution
# mtl must be taken from repo not from package-b
grep -q @stackage//:mtl package-a/BUILD.bazel
! grep -q //package-b:mtl package-a/BUILD.bazel
# Test that colon syntax allows to reference both
# local and other package dependency
grep -q :mtl package-b/BUILD.bazel
grep -q //package-c:mtl package-b/BUILD.bazel
# Test gazelle-update-repos
nix-shell --pure --run "bazel run //:gazelle-update-repos --config=$GHC_VERSION"
nix-shell --pure --run "bazel test //... --config=$GHC_VERSION"
