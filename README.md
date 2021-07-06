## A gazelle extension for cabal files

This is a [gazelle][gazelle] extension that generates and
updates [Haskell rules][rules_haskell] for [bazel][bazel]
from [Cabal][cabal] files.

For each cabal file found, rules are generated in a sibling
`BUILD` file for every buildable component.

Additionally, it generates a `stack_snapshot` rule in the
`WORKSPACE` file with all of the necessary dependencies.

This [example](example) shows it in action.

## Configuration

Firstly, setup [gazelle][gazelle] and [rules_haskell][rules_haskell].
Then import `gazelle_cabal`.

```python
http_archive(
    name = "io_tweag_cabal_gazelle",
    strip_prefix = "rules_haskell-main",
    url = "https://github.com/tweag/gazelle_cabal/archive/main.zip",
)
```

Additionally, a `stack_snapshot` rule is needed in the `WORKSPACE` file
to provide the necessary Haskell dependencies as follows.

```python
load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    packages = [
        "aeson", # keep
        "optparse-applicative", # keep
        "path", # keep
        "path-io", # keep
    ],
    # Most snapshots of your choice might do
    snapshot = "lts-18.1",
)
```

You can generate or update build rules by adding the following to
one of your `BUILD` files.

```python
load(
    "@bazel_gazelle//:def.bzl",
    "DEFAULT_LANGUAGES",
    "gazelle",
    "gazelle_binary",
)

gazelle(
    name = "gazelle",
    data = ["@io_tweag_gazelle_cabal//cabalscan"],
    gazelle = "//:gazelle_binary",
)

gazelle_binary(
    name = "gazelle_binary",
    languages = DEFAULT_LANGUAGES + ["@io_tweag_gazelle_cabal//gazelle_cabal"],
)
```
Then build and run gazelle with
```bash
bazel run //:gazelle
```

In order to generate or update the `stack_snapshot` rule, add
the following to one of your build files

```python
gazelle(
    name = "gazelle-update-repos",
    command = "update-repos",
    data = ["@io_tweag_gazelle_cabal//cabalscan"],
    extra_args = [
        "-lang",
        "gazelle_cabal",
        "stackage",
    ],
    gazelle = "//:gazelle_binary",
)
```
Then build and run gazelle with
```bash
bazel run //:gazelle-update-repos
```
`gazelle_cabal` only fills the `packages` attribute of `stack_snapshot`.
Either before or after the update, the other mandatory attributes of the
rule need to be written by the user.

## Directives

```python
# gazelle:cabal_extra_libraries sodium=@libsodium//:libsodium
```
Maps names in the `extra-libraries` field to bazel labels. The
labels are added to the `deps` attribute of the corresponding rule.
Unmapped names are added to the `compiler_flags` attribute
as `-l<name>`.

## Dependency resolution

In general, package names in the `build-depends` field are mapped to
`@stackage//:<package_name>`, except if there is a `haskell_library`
rule in the current repository with the same name, in which
case such a target is added to the `deps` attribute instead.

If there is a `ghc_plugin` rule named as `<package name>-plugin` and
`<package name>` is listed in the `build-depends` field, the
corresponding label is added to the `plugins` attribute and omitted
in `deps`.

Tools in the `build-tool-depends` field are mapped to
`@stackage-exe//<package_name>:<executable_name>` and added to the
`tools` attribute, unless there is a `haskell_binary` rule in the
current repo named as the executable, in which case such a target
is added to the `tools` attribute instead. Currently, any needed
tools have to be added manually to the `components` attribute of
the `stack_snapshot` rule.

Files in the `data-files` field are placed verbatim in the `data`
attribute of the `haskell_library` rule if a library component is
present in the cabal file. Otherwise, the `data-files` field is
ignored.

## Implementation

`gazelle_cabal` extracts data from Cabal files using [cabalscan][cabalscan],
a command line tool written in Haskell, which presents the extracted data
in json format to the `go` part.

The [go part][go-part] consists of a set of functions written in the
[go][go] programming language, which `gazelle` will invoke to
generate Haskell rules. The most important functions are:

* `GenerateRules`: calls the `cabalscan` command-line tool and
  produces rules that contain no information about dependencies.

* `Resolve`: adds to the previously generated rules all the
  information about dependencies (`deps`, `plugins`, and `tools`).

* `UpdateRepos`: scans the `BUILD` files for haskell rules, extracts
  their dependencies, and puts them in a `stack_snapshot` rule in the
  `WORKSPACE` file.

## Limitations

Despite that `gazelle_cabal` can produce most of the build configuration
from cabal files, Haskell dependencies brought with `stack_snapshot`
might fail to build if their cabal files use sublibraries or some particular
custom `Setup.hs` files. In these cases, the simpler route to adoption could
be to patch the problematic dependencies and add them to a local `stack`
snapshot (see the [local_snapshot][local_snapshot] attribute of
`stack_snapshot`).

## Sponsors

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![Symbiont](https://imgur.com/KPV3lTY.png)](https://symbiont.io)
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![Tweag I/O](http://i.imgur.com/0HK8X4y.png)](http://tweag.io)

`gazelle_cabal` was funded by [Symbiont](https://www.symbiont.io/)
and is maintained by [Tweag I/O](http://tweag.io/).

Have questions? Need help? Tweet at
[@tweagio](http://twitter.com/tweagio).

[bazel]: https://bazel.build
[cabal]: https://www.haskell.org/cabal
[cabalscan]: cabalscan/exe/Main.hs
[gazelle]: https://github.com/bazelbuild/bazel-gazelle
[go-part]: gazelle_cabal/lang.go
[go]: https://golang.org
[rules_haskell]: https://github.com/tweag/rules_haskell
[local_snapshot]: https://api.haskell.build/haskell/cabal.html#stack_snapshot-local_snapshot
