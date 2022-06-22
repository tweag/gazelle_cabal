""" Implementation of gazelle_cabal_dependencies """

def _gazelle_cabal_dependencies_impl(repository_ctx):
    repository_ctx.file(
        "BUILD",
        content = '''
package(default_visibility = ["//visibility:public"])

alias(name="json", actual="{json}")
alias(name="path", actual="{path}")
alias(name="path-io", actual="{path_io}")
        '''.format(
            json = repository_ctx.attr.json,
            path = repository_ctx.attr.path,
            path_io = repository_ctx.attr.path_io,
        ),
        executable = False,
    )

_gazelle_cabal_dependencies = repository_rule(
    implementation = _gazelle_cabal_dependencies_impl,
    local = True,
    attrs = {
        "json": attr.label(default = "@stackage//:json"),
        "path": attr.label(default = "@stackage//:path"),
        "path_io": attr.label(default = "@stackage//:path-io"),
    },
)

def gazelle_cabal_dependencies(**kargs):
    """
    Produces a repository with the dependencies of cabalscan.

    The main purpose of it is to offer a convenient way to override
    the dependencies that cabalscan uses. By default, all dependencies
    are assumed to come from a @stackage repository.

    Example:

      ```bzl
      # Dependencies taken from @stackage
      gazelle_cabal_dependencies()

      # Dependencies overriden
      gazelle_cabal_dependencies(
          json = "@someother//:some-other-json",
          path = "@someother//:some-path",
          path_io = "@someother//:another-path-io",
      )

      ```

    """
    _gazelle_cabal_dependencies(name = "io_tweag_gazelle_cabal_deps", **kargs)
