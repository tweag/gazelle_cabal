This is an example project using the [gazelle_cabal][gazelle_cabal] extension.

Run the following to generate the build configuration from the cabal files.
```bazel
nix-shell --pure --run "bazel build //:gazelle"
nix-shell --pure --run "bazel build //:gazelle-update-repos"
```
Now you can build and test with
```bazel
nix-shell --pure --run "bazel build //..."
nix-shell --pure --run "bazel test //..."
```

[gazelle_cabal]: https://github.com/tweag/gazelle_cabal
