# Contributing to `gazelle_cabal`

## Bazel Remote Cache

The remote cache configuration for this repository is stored in [.bazelrc](/.bazelrc) and grouped
under the name, `remote-cache`. It is configured to allow read-only access for all clients and
read-write for CI. 

To enable the remote cache, 

1. Add `build --remote_header=x-buildbuddy-api-key=${buildbuddy_api_key}` to `.bazelrc.auth`
   at the root of the workspace, replacing `${buildbuddy_api_key}` with the actual API key value.
1. Add `build --config=remote-cache` to `.bazelrc.local` at the root of the workspace.
