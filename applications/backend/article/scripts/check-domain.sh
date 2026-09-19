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

assert_coverage() {
  local label="$1"
  shift
  local report counts covered total
  report="$(hpc report "${tix}" "--hpcdir=${mix}" "$@")"
  printf '%s\n' "${report}"
  counts="$(sed -nE 's/.*expressions used \(([0-9]+)\/([0-9]+)\).*/\1 \2/p' <<<"${report}")"
  read -r covered total <<<"${counts}"
  if [[ -z "${covered:-}" || -z "${total:-}" || "${total}" -eq 0 ]]; then
    echo "${label}: coverage could not be measured" >&2
    exit 1
  fi
  awk -v label="${label}" -v covered="${covered}" -v total="${total}" \
    'BEGIN { printf "%s: %.2f%% (%d/%d expressions)\n", label, covered*100/total, covered, total }'
  if ((covered * 100 < total * 90)); then
    echo "${label}: coverage is below 90%" >&2
    exit 1
  fi
}

assert_coverage "Article domain" \
  --include=Domain.Article \
  --include=Domain.Article.Common \
  --include=Domain.Article.Draft \
  --include=Domain.Article.Published \
  --include=Domain.Article.Private \
  --include=Domain.Article.Event

assert_coverage "Article use cases" \
  --include=UseCase.JotDown \
  --include=UseCase.AmendDraft \
  --include=UseCase.Proofread \
  --include=UseCase.PrepareToPublish \
  --include=UseCase.Publish \
  --include=UseCase.TakeDown \
  --include=UseCase.ResumePublication \
  --include=UseCase.DiscardArticle \
  --include=UseCase.Reading \
  --include=UseCase.BrowseArticlesForAdmin \
  --include=UseCase.ViewArticleForAdmin \
  --include=UseCase.BrowseArticlesForReader \
  --include=UseCase.ReadArticle \
  --include=UseCase.CheckSlugAvailability \
  --include=UseCase.Persistence \
  --include=UseCase.Result

fixtures="applications/backend/article/test/unit/typecheck"
assert_coverage "Article reading use cases" \
  --include=UseCase.Reading \
  --include=UseCase.BrowseArticlesForAdmin \
  --include=UseCase.ViewArticleForAdmin \
  --include=UseCase.BrowseArticlesForReader \
  --include=UseCase.ReadArticle \
  --include=UseCase.CheckSlugAvailability

(
  binary="$(cabal list-bin --builddir="${build_directory}" shared:shared-test --enable-coverage --disable-optimization)"
  component_root="${binary%/build/shared-test/shared-test}"
  package_root="${component_root%%/t/*}"
  profile="${component_root#"${package_root}/t/shared-test"}"
  mix="${package_root}${profile}/build/extra-compilation-artifacts/hpc/vanilla/mix"
  tix="${component_root}/hpc/vanilla/tix/shared-test.tix"
  assert_coverage "Shared Pager" --include=Shared.Domain.Pager
)

compile() {
  cabal exec --builddir="${build_directory}" --enable-coverage --disable-optimization \
    -- ghc -fno-code -XGHC2024 -XDataKinds -XOverloadedStrings \
    -package article "$1"
}
compile "${fixtures}/ValidPublish.hs"
for fixture in InvalidPublish InvalidCoerce InvalidContentUpdate InvalidDraftEvent \
  InvalidPublishEvent InvalidTakeDownEvent InvalidResumeEvent InvalidDiscardEvent \
  InvalidBrowseArticlesForAdminEvent InvalidBrowseArticlesForReaderEvent \
  InvalidViewArticleForAdminEvent InvalidReadArticleEvent InvalidCheckSlugAvailabilityEvent; do
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
