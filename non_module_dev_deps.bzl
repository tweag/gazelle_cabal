load(
    "@rules_nixpkgs_core//:nixpkgs.bzl",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)
load("@rules_nixpkgs_cc//:cc.bzl", "nixpkgs_cc_configure")

def _non_module_dev_deps_impl(_ctx):
    nixpkgs_local_repository(
        name = "nixpkgs",
        nix_file = "//:nixpkgs.nix",
    )

    nixpkgs_cc_configure(
        name = "nixpkgs_config_cc",
        repository = "@nixpkgs",
        register = False,
    )

non_module_dev_deps = module_extension(
    implementation = _non_module_dev_deps_impl,
)
