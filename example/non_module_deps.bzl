load("@rules_nixpkgs_cc//:cc.bzl", "nixpkgs_cc_configure")
load(
    "@rules_nixpkgs_core//:nixpkgs.bzl",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)

def _non_module_deps_impl(_ctx):
    nixpkgs_local_repository(
        name = "nixpkgs",
        nix_file = "//:nixpkgs.nix",
    )

    nixpkgs_cc_configure(
        name = "nixpkgs_config_cc",
        repository = "@nixpkgs",
        register = False,
    )

    nixpkgs_package(
        name = "nixpkgs_zlib",
        attribute_path = "zlib",
        repository = "@nixpkgs",
    )

    nixpkgs_package(
        name = "zlib.dev",
        build_file_content = """
load("@rules_cc//cc:defs.bzl", "cc_library")

filegroup(
    name = "include",
    srcs = glob(["include/*.h"]),
    visibility = ["//visibility:public"],
)

cc_library(
    name = "zlib",
    srcs = ["@nixpkgs_zlib//:lib"],
    hdrs = [":include"],
    strip_include_prefix = "include",
    visibility = ["//visibility:public"],
    # This rule only bundles headers and a library and doesn't compile or link by itself.
    # We set linkstatic = 1 to quiet to quiet the following warning:
    #
    #   in linkstatic attribute of cc_library rule @zlib.dev//:zlib:
    #   setting 'linkstatic=1' is recommended if there are no object files.
    #
    linkstatic = 1,
)
""",
        repository = "@nixpkgs",
    )

non_module_deps = module_extension(
    implementation = _non_module_deps_impl,
)
