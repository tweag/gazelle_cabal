set -euo pipefail

if [[ $# -ne 2 || ! $1 =~ ^ghc_[0-9]+_[0-9]+_[0-9]+$ || ! $2 =~ ^(true|false)$ ]]; then
  echo "Usage: $0 ghc_X_Y_Z true|false" >&2
  exit 1
fi

ghc_version=$1
enable_bzlmod=$2

GHC_VERSION="$(sed 's/^ghc_//;s/_/./g' <<<"$ghc_version")"

bzlmod_setting="common --noenable_bzlmod"
if [[ $enable_bzlmod = true ]]; then
  bzlmod_setting="common --config=bzlmod"
  # Update the GHC_VERSION in MODULE.bazel.
  sed -i.bak 's/^GHC_VERSION = .*$/GHC_VERSION = "'"$GHC_VERSION"'"/g' example/MODULE.bazel
fi

# Ensure the local Bazel configuration file exists, 
# then remove the two config lines being regulated here, if present.
touch .bazelrc.local
sed -i '/^common --config=bzlmod$/d; /^common --noenable_bzlmod$/d' .bazelrc.local
# If needed, add the repo cache configuration line.
if ! grep -qxF 'build --repository_cache=~/repo-cache/' .bazelrc.local; then
  echo 'build --repository_cache=~/repo-cache/' >>.bazelrc.local
fi
# Write the current settings and propagate them to the use sites.
echo "$bzlmod_setting" >>.bazelrc.local
ln -sfn ../.bazelrc.local example/.bazelrc.local
ln -sfn ../../.bazelrc.local tests/alternative-deps/.bazelrc.local
