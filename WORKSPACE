workspace(name = "io_tweag_gazelle_cabal")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

##########################
# rules_nixpkgs preamble
##########################

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-0.11.1",
    urls = ["https://github.com/tweag/rules_nixpkgs/releases/download/v0.11.1/rules_nixpkgs-0.11.1.tar.gz"],
    sha256 = "2a555348d7f8593fca2bf3fc6ce53c5d62929de81b6c292e23f16c557c0ae45a",
)

http_archive(
    name = "rules_nixpkgs_core",
    strip_prefix = "rules_nixpkgs-0.11.1/core",
    urls = ["https://github.com/tweag/rules_nixpkgs/releases/download/v0.11.1/rules_nixpkgs-0.11.1.tar.gz"],
    sha256 = "2a555348d7f8593fca2bf3fc6ce53c5d62929de81b6c292e23f16c557c0ae45a",
)

[
    http_archive(
        name = "rules_nixpkgs_" + toolchain,
        strip_prefix = "rules_nixpkgs-0.11.1/toolchains/" + toolchain,
        urls = ["https://github.com/tweag/rules_nixpkgs/releases/download/v0.11.1/rules_nixpkgs-0.11.1.tar.gz"],
        sha256 = "2a555348d7f8593fca2bf3fc6ce53c5d62929de81b6c292e23f16c557c0ae45a",
    )
    for toolchain in ["python", "go", "posix"]
]

##########################
# rules_haskell preamble
##########################

http_archive(
    name = "rules_haskell",
    sha256 = "34742848a8882d94a0437b3b1917dea6f58c82fe5762afe8d249d3a36e51935d",
    strip_prefix = "rules_haskell-0.19",
    urls = ["https://github.com/tweag/rules_haskell/releases/download/v0.19/rules_haskell-0.19.tar.gz"],
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load("@rules_nixpkgs_core//:nixpkgs.bzl", "nixpkgs_local_repository")

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//:nixpkgs.nix",
)

load("@rules_nixpkgs_python//:python.bzl", "nixpkgs_python_configure")

nixpkgs_python_configure(repository = "@nixpkgs")

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

######################################
# Haskell dependencies and toolchain
######################################

load("//:config_settings/setup.bzl", "config_settings")

config_settings(name = "config_settings")

load("@config_settings//:info.bzl", "ghc_version")
load("//:defs.bzl", "gazelle_cabal_dependencies")

gazelle_cabal_dependencies()

stack_snapshot(
    name = "stackage",
    components =
        {
            "tasty-discover": [
                "lib",
                "exe:tasty-discover",
            ],
        },
    local_snapshot = "//:snapshot-" + ghc_version + ".yaml",
    packages = [
        "Cabal",
        "directory",
        "filepath",
        "hspec",
        "json",
        "tasty",
        "tasty-discover",
        "tasty-hspec",
        "temporary",
    ] +
    # downgrade logict from 0.7.1.0 for GHC 8.10 and 9.0 which is not resolvable
    # from hackage in the latest revision (see https://github.com/Bodigrim/logict/issues/20)
    (["logict-0.7.0.3"] if ghc_version in [
        "8.10.7",
        "9.0.2",
    ] else []),
    setup_deps = {
        "call-stack": ["@stackage//:Cabal"],
        "hspec": ["@stackage//:Cabal"],
        "hspec-core": ["@stackage//:Cabal"],
        "hspec-discover": ["@stackage//:Cabal"],
        "hspec-expectations": ["@stackage//:Cabal"],
        "HUnit": ["@stackage//:Cabal"],
        "quickcheck-io": ["@stackage//:Cabal"],
        "tasty-discover": ["@stackage//:Cabal"],
        "transformers-compat": ["@stackage//:Cabal"],
    },
    stack_snapshot_json = "//:snapshot-" + ghc_version + ".json",
)

load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")

haskell_register_ghc_nixpkgs(
    attribute_path =
        "haskell.compiler.ghc" + ghc_version.replace(".", ""),
    ghcopts = [
        "-Werror",
        "-Wall",
        "-Wcompat",
        "-Wincomplete-record-updates",
        "-Wredundant-constraints",
    ],
    repositories = {"nixpkgs": "@nixpkgs"},
    version = ghc_version,
)

###############
# Go preamble
###############

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "b2038e2de2cace18f032249cb4bb0048abf583a36369fa98f687af1b3f880b26",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.48.1/rules_go-v0.48.1.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.48.1/rules_go-v0.48.1.zip",
    ],
)

load("@rules_nixpkgs_go//:go.bzl", "nixpkgs_go_configure")

nixpkgs_go_configure(
    attribute_path = "go_1_20",
    repository = "@nixpkgs",
)

load("@io_bazel_rules_go//go:deps.bzl", "go_rules_dependencies")

go_rules_dependencies()

####################
# Gazelle preamble
####################

http_archive(
    name = "bazel_gazelle",
    sha256 = "d76bf7a60fd8b050444090dfa2837a4eaf9829e1165618ee35dceca5cbdf58d5",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.37.0/bazel-gazelle-v0.37.0.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.37.0/bazel-gazelle-v0.37.0.tar.gz",
    ],
)

load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies", "go_repository")

# go_repository added due to: https://github.com/bazelbuild/bazel-gazelle/issues/1217
go_repository(
    name = "org_golang_x_xerrors",
    importpath = "golang.org/x/xerrors",
    sum = "h1:go1bK/D/BFZV2I8cIQd1NKEZ+0owSTG1fDTci4IqFcE=",
    version = "v0.0.0-20200804184101-5ec99f83aff1",
)

gazelle_dependencies()

#######################
# Buildifier preamble
#######################

http_archive(
    name = "rules_proto",
    sha256 = "6fb6767d1bef535310547e03247f7518b03487740c11b6c6adb7952033fe1295",
    strip_prefix = "rules_proto-6.0.2",
    url = "https://github.com/bazelbuild/rules_proto/releases/download/6.0.2/rules_proto-6.0.2.tar.gz",
)

load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies")
rules_proto_dependencies()

load("@rules_proto//proto:setup.bzl", "rules_proto_setup")
rules_proto_setup()

load("@rules_proto//proto:toolchains.bzl", "rules_proto_toolchains")
rules_proto_toolchains()

http_archive(
    name = "com_github_bazelbuild_buildtools",
    sha256 = "05c3c3602d25aeda1e9dbc91d3b66e624c1f9fdadf273e5480b489e744ca7269",
    strip_prefix = "buildtools-6.4.0",
    urls = [
        "https://github.com/bazelbuild/buildtools/archive/refs/tags/v6.4.0.tar.gz",
    ],
)
