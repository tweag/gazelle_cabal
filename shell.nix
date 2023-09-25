{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

mkShell {
  # XXX: hack for macosX, this flags disable bazel usage of xcode
  # Note: this is set even for linux so any regression introduced by this flag
  # will be catched earlier
  # See: https://github.com/bazelbuild/bazel/issues/4231
  BAZEL_USE_CPP_ONLY_TOOLCHAIN=1;
  buildInputs = [
    bazel_5
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

