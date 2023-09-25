workspace(name = "io_tweag_gazelle_cabal")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

##########################
# rules_haskell preamble
##########################

http_archive(
    name = "rules_haskell",
    sha256 = "1485a90366039fe8ea5051732dc50827d8c1e9bb6feb2c8bb1ad1fed5344cff0",
    strip_prefix = "rules_haskell-155ba21c7d315cc421ee3f17a1c09b758cc7279d",
    urls = ["https://github.com/tweag/rules_haskell/archive/155ba21c7d315cc421ee3f17a1c09b758cc7279d.tar.gz"],
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_local_repository",
    "nixpkgs_python_configure",
)

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//:nixpkgs.nix",
)

nixpkgs_python_configure(repository = "@nixpkgs")

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

######################################
# Haskell dependencies and toolchain
######################################

load("//:config_settings/setup.bzl", "config_settings")

config_settings(name = "config_settings")

load("@config_settings//:info.bzl", "ghc_version")
load("@io_tweag_gazelle_cabal//:defs.bzl", "gazelle_cabal_dependencies")

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
    sha256 = "278b7ff5a826f3dc10f04feaf0b70d48b68748ccd512d7f98bf442077f043fe3",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.41.0/rules_go-v0.41.0.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.41.0/rules_go-v0.41.0.zip",
    ],
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:toolchains/go.bzl",
    "nixpkgs_go_configure",
)

nixpkgs_go_configure(repository = "@nixpkgs")

load("@io_bazel_rules_go//go:deps.bzl", "go_rules_dependencies")

go_rules_dependencies()

####################
# Gazelle preamble
####################

http_archive(
    name = "bazel_gazelle",
    sha256 = "d3fa66a39028e97d76f9e2db8f1b0c11c099e8e01bf363a923074784e451f809",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.33.0/bazel-gazelle-v0.33.0.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.33.0/bazel-gazelle-v0.33.0.tar.gz",
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
    name = "com_github_bazelbuild_buildtools",
    sha256 = "5b7fe9aa131ab64a51de4da3668005cf58418c967438ce129aad24fd3e6dfaa9",
    strip_prefix = "buildtools-4890966c38b910fd5bd1ad78a3dd88538d09854f",
    url = "https://github.com/bazelbuild/buildtools/archive/4890966c38b910fd5bd1ad78a3dd88538d09854f.zip",
)
