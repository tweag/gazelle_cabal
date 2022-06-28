""" Implementation of gazelle_cabal_dependencies """

def _gazelle_cabal_dependencies_impl(repository_ctx):
    repository_ctx.file(
        "BUILD",
        content = '''
package(default_visibility = ["//visibility:public"])

alias(name="json", actual="{json}")
alias(name="filepath", actual="{filepath}")
alias(name="directory", actual="{directory}")
        '''.format(
            json = repository_ctx.attr.json,
            filepath = repository_ctx.attr.filepath,
            directory = repository_ctx.attr.directory,
        ),
        executable = False,
    )

_gazelle_cabal_dependencies = repository_rule(
    implementation = _gazelle_cabal_dependencies_impl,
    local = True,
    attrs = {
        "json": attr.label(default = "@stackage//:json"),
        "filepath": attr.label(default = "@stackage//:filepath"),
        "directory": attr.label(default = "@stackage//:directory"),
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
          filepath = "@someother//:some-filepath",
          directory = "@someother//:another-directory",
      )

      ```

    """
    _gazelle_cabal_dependencies(name = "io_tweag_gazelle_cabal_deps", **kargs)
