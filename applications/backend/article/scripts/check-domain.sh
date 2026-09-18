#!/usr/bin/env bash
set -euo pipefail

script_directory="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repository_root="$(cd "${script_directory}/../../../.." && pwd)"
cd "${repository_root}"
build_directory="${repository_root}/_build/article-domain"

cabal test --builddir="${build_directory}" article:article-unit-test shared:shared-test \
  --enable-coverage --disable-optimization --test-show-details=direct

binary="$(cabal list-bin --builddir="${build_directory}" article:article-unit-test --enable-coverage --disable-optimization)"
component_root="${binary%/build/article-unit-test/article-unit-test}"
package_root="${component_root%%/t/*}"
profile="${component_root#"${package_root}/t/article-unit-test"}"
mix="${package_root}${profile}/build/extra-compilation-artifacts/hpc/vanilla/mix"
tix="${component_root}/hpc/vanilla/tix/article-unit-test.tix"

report="$(hpc report "${tix}" "--hpcdir=${mix}" \
  --include=Domain.Article \
  --include=Domain.Article.Common \
  --include=Domain.Article.Draft \
  --include=Domain.Article.Published \
  --include=Domain.Article.Private)"
printf '%s\n' "${report}"
counts="$(sed -nE 's/.*expressions used \(([0-9]+)\/([0-9]+)\).*/\1 \2/p' <<<"${report}")"
read -r covered total <<<"${counts}"
if [[ -z "${covered:-}" || -z "${total:-}" || "${total}" -eq 0 ]]; then
  echo "Article domain coverage could not be measured" >&2
  exit 1
fi
awk -v covered="${covered}" -v total="${total}" \
  'BEGIN { printf "Article domain: %.2f%% (%d/%d expressions)\n", covered*100/total, covered, total }'
if ((covered * 100 < total * 90)); then
  echo "Article domain coverage is below 90%" >&2
  exit 1
fi

fixtures="applications/backend/article/test/unit/typecheck"
compile() {
  cabal exec --builddir="${build_directory}" --enable-coverage --disable-optimization \
    -- ghc -fno-code -XGHC2024 -XDataKinds -XOverloadedStrings \
    -package article "$1"
}
compile "${fixtures}/ValidPublish.hs"
for fixture in InvalidPublish InvalidCoerce InvalidContentUpdate; do
  expected_error=GHC-83865
  if [[ "${fixture}" == InvalidCoerce ]]; then
    expected_error=GHC-18872
  fi
  if output="$(compile "${fixtures}/${fixture}.hs" 2>&1)"; then
    echo "${fixture}: unexpectedly compiled" >&2
    exit 1
  fi
  if ! grep -q "${expected_error}" <<<"${output}"; then
    printf '%s\n' "${output}" >&2
    echo "${fixture}: failed for an unexpected reason" >&2
    exit 1
  fi
  echo "${fixture}: rejected as expected"
done
