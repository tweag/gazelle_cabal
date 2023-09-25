This is an example project using the [gazelle_cabal][gazelle_cabal] extension.

Run the following to generate the build configuration from the Cabal files.
```bazel

echo "build --host_platform=@rules_nixpkgs_core//platforms:host" > .bazelrc.local
nix-shell --pure --run "bazel run //:gazelle"
nix-shell --pure --run "bazel run //:gazelle-update-repos"
```
Now you can build and test with
```bazel
nix-shell --pure --run "bazel build //..."
nix-shell --pure --run "bazel test //..."
```

[gazelle_cabal]: https://github.com/tweag/gazelle_cabal
