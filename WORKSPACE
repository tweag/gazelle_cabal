workspace(name = "io_tweag_gazelle_cabal")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

##########################
# rules_java preamble
##########################

http_archive(
    name = "rules_proto",
    sha256 = "14a225870ab4e91869652cfd69ef2028277fc1dc4910d65d353b62d6e0ae21f4",
    strip_prefix = "rules_proto-7.1.0",
    url = "https://github.com/bazelbuild/rules_proto/releases/download/7.1.0/rules_proto-7.1.0.tar.gz",
)

load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies")

rules_proto_dependencies()

load("@rules_proto//proto:setup.bzl", "rules_proto_setup")

rules_proto_setup()

load("@rules_proto//proto:toolchains.bzl", "rules_proto_toolchains")

rules_proto_toolchains()

http_archive(
    name = "rules_java",
    sha256 = "19008f8a85125c9476ef37b6ad945f665d7178aaab3746f7962917ccd87d2477",
    urls = [
        "https://github.com/bazelbuild/rules_java/releases/download/9.0.0/rules_java-9.0.0.tar.gz",
    ],
)

load("@rules_java//java:rules_java_deps.bzl", "rules_java_dependencies")

rules_java_dependencies()

load("@rules_java//java:repositories.bzl", "rules_java_toolchains")

rules_java_toolchains()

##########################
# rules_nixpkgs preamble
##########################

http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = "30271f7bd380e4e20e4d7132c324946c4fdbc31ebe0bbb6638a0f61a37e74397",
    strip_prefix = "rules_nixpkgs-0.13.0",
    urls = ["https://github.com/tweag/rules_nixpkgs/releases/download/v0.13.0/rules_nixpkgs-0.13.0.tar.gz"],
)

http_archive(
    name = "rules_nixpkgs_core",
    sha256 = "30271f7bd380e4e20e4d7132c324946c4fdbc31ebe0bbb6638a0f61a37e74397",
    strip_prefix = "rules_nixpkgs-0.13.0/core",
    urls = ["https://github.com/tweag/rules_nixpkgs/releases/download/v0.13.0/rules_nixpkgs-0.13.0.tar.gz"],
)

[
    http_archive(
        name = "rules_nixpkgs_" + toolchain,
        sha256 = "30271f7bd380e4e20e4d7132c324946c4fdbc31ebe0bbb6638a0f61a37e74397",
        strip_prefix = "rules_nixpkgs-0.13.0/toolchains/" + toolchain,
        urls = ["https://github.com/tweag/rules_nixpkgs/releases/download/v0.13.0/rules_nixpkgs-0.13.0.tar.gz"],
    )
    for toolchain in [
        "python",
        "go",
        "posix",
    ]
]

##########################
# rules_haskell preamble
##########################

http_archive(
    name = "rules_haskell",
    sha256 = "4cae22bc84f327bf3cb7605021c3663160ff6bc8a0b7b6266062366bcbd19e79",
    strip_prefix = "rules_haskell-1.0",
    urls = ["https://github.com/tweag/rules_haskell/releases/download/v1.0/rules_haskell-1.0.tar.gz"],
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
    sha256 = "89d2050410602142c9acafd01c95baf48b65f8dd16f4771d37c89f82f5e147f2",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.56.1/rules_go-v0.56.1.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.56.1/rules_go-v0.56.1.zip",
    ],
)

load("@rules_nixpkgs_go//:go.bzl", "nixpkgs_go_configure")

nixpkgs_go_configure(
    repository = "@nixpkgs",
)

load("@io_bazel_rules_go//go:deps.bzl", "go_rules_dependencies")

go_rules_dependencies()

####################
# Gazelle preamble
####################

http_archive(
    name = "bazel_gazelle",
    sha256 = "efd44914900fe52eed891b5e11ff443e8b18ea9b84e304213f04d18e8e20df3d",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.46.0/bazel-gazelle-v0.46.0.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.46.0/bazel-gazelle-v0.46.0.tar.gz",
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
    sha256 = "53119397bbce1cd7e4c590e117dcda343c2086199de62932106c80733526c261",
    strip_prefix = "buildtools-8.2.1",
    urls = [
        "https://github.com/bazelbuild/buildtools/archive/refs/tags/v8.2.1.tar.gz",
    ],
)
