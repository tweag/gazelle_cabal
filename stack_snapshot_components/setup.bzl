
def _components_impl(repository_ctx):
    csplit = [ c.split(" ") for c in repository_ctx.attr.components ]
    splitlist = { c[0]: c[1:] for c in csplit }
    repository_ctx.file(
        "BUILD",
        content = """exports_files(["components.bzl"])""",
        executable = False,
    )
    repository_ctx.file(
        "components.bzl",
        content = 'COMPONENTS = "{}"'.format(splitlist),
        executable = False,
    )

stack_snapshot_components = repository_rule(
    implementation=_components_impl,
    local=True,
    attrs={"components": attr.string_list(mandatory=True)},
)
