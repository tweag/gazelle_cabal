load("//:defs.bzl", "gazelle_cabal_dependencies")

def _non_module_deps_impl(_ctx):
    # TODO[AH]: This is only required for backward compatibility to WORKSPACE mode.
    #   Once WORKSPACE mode support is removed we can directly reference the
    #   `@stackage//...` labels for the dependencies introduced by this macro.
    gazelle_cabal_dependencies()

non_module_deps = module_extension(
    implementation = _non_module_deps_impl,
)
