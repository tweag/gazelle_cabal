{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

mkShell {
  # XXX: hack for macosX, this flag disables bazel usage of xcode
  # Note: this is set even for linux so any regression introduced by this flag
  # will be caught earlier.
  # See: https://github.com/bazelbuild/bazel/issues/4231
  BAZEL_USE_CPP_ONLY_TOOLCHAIN=1;
  
  buildInputs = [
    bazel_6
    binutils
    cacert
    nix
    git
    openjdk11
    python3
    stack
    # convenience dependencies
    less
  ];
}

