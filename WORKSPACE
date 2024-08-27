workspace(name = "io_tweag_gazelle_cabal")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

##########################
# rules_nixpkgs preamble
##########################

http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = "1adb04dc0416915fef427757f4272c4f7dacefeceeefc50f683aec7f7e9b787a",
    strip_prefix = "rules_nixpkgs-0.12.0",
    urls = ["https://github.com/tweag/rules_nixpkgs/releases/download/v0.12.0/rules_nixpkgs-0.12.0.tar.gz"],
)

http_archive(
    name = "rules_nixpkgs_core",
    sha256 = "1adb04dc0416915fef427757f4272c4f7dacefeceeefc50f683aec7f7e9b787a",
    strip_prefix = "rules_nixpkgs-0.12.0/core",
    urls = ["https://github.com/tweag/rules_nixpkgs/releases/download/v0.12.0/rules_nixpkgs-0.12.0.tar.gz"],
)

[
    http_archive(
        name = "rules_nixpkgs_" + toolchain,
        sha256 = "1adb04dc0416915fef427757f4272c4f7dacefeceeefc50f683aec7f7e9b787a",
        strip_prefix = "rules_nixpkgs-0.12.0/toolchains/" + toolchain,
        urls = ["https://github.com/tweag/rules_nixpkgs/releases/download/v0.12.0/rules_nixpkgs-0.12.0.tar.gz"],
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
    sha256 = "d93ef02f1e72c82d8bb3d5169519b36167b33cf68c252525e3b9d3d5dd143de7",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.49.0/rules_go-v0.49.0.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.49.0/rules_go-v0.49.0.zip",
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
    sha256 = "8ad77552825b078a10ad960bec6ef77d2ff8ec70faef2fd038db713f410f5d87",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.38.0/bazel-gazelle-v0.38.0.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.38.0/bazel-gazelle-v0.38.0.tar.gz",
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
    sha256 = "051951c10ff8addeb4f10be3b0cf474b304b2ccd675f2cc7683cdd9010320ca9",
    strip_prefix = "buildtools-7.3.1",
    urls = [
        "https://github.com/bazelbuild/buildtools/archive/refs/tags/v7.3.1.tar.gz",
    ],
)
