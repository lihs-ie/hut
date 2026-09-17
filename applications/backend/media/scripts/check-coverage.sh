#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIRECTORY="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPT_DIRECTORY
REPOSITORY_ROOT="$(cd "${SCRIPT_DIRECTORY}/../../../.." && pwd)"
readonly REPOSITORY_ROOT
readonly COVERAGE_THRESHOLD=90

readonly UNIT_TEST="media-unit-test"

readonly DOMAIN_MODULES=(
  Media.Domain.Image
  Media.Domain.Image.Event
  Media.Domain.Image.Inspection
  Media.Domain.Image.Retention
  Media.Domain.ImageUsage
)

readonly USE_CASE_MODULES=(
  Media.Internal.Result
  Media.Internal.UploadDestination
  Media.UseCase.GetImageStatus
  Media.UseCase.InspectImage
  Media.UseCase.ProcessImageInspection
  Media.UseCase.ProjectImageUsage
  Media.UseCase.RequestImageUpload
  Media.UseCase.Result
  Media.UseCase.RetainImages
  Media.UseCase.RetryImageInspection
  Media.UseCase.RetryImageUpload
)

readonly PRESENTATION_MODULES=(
  Media.Presentation.API
  Media.Presentation.API.GetImageStatus
  Media.Presentation.API.RequestImageUpload
  Media.Presentation.API.RetryImageInspection
  Media.Presentation.API.RetryImageUpload
  Media.Presentation.Handler.API.Error
  Media.Presentation.Handler.API.GetImageStatus
  Media.Presentation.Handler.API.Metadata
  Media.Presentation.Handler.API.RequestImageUpload
  Media.Presentation.Handler.API.RetryImageInspection
  Media.Presentation.Handler.API.RetryImageUpload
  Media.Presentation.Handler.InspectionQueue
  Media.Presentation.Handler.ReferenceProjectionQueue
  Media.Presentation.Handler.RetentionScheduled
  Media.Presentation.Server.API
)

coverage_report() {
  local tix_file="$1"
  shift
  local include_flags=()
  local module

  for module in "$@"; do
    include_flags+=("--include=${module}")
  done

  hpc report \
    "${tix_file}" \
    "--hpcdir=${CORE_MIX_DIRECTORY}" \
    "--hpcdir=${CORE_PACKAGE_MIX_DIRECTORY}" \
    "--hpcdir=${PRESENTATION_MIX_DIRECTORY}" \
    "--hpcdir=${PRESENTATION_PACKAGE_MIX_DIRECTORY}" \
    "${include_flags[@]}"
}

assert_expression_coverage() {
  local label="$1"
  local report="$2"
  local counts
  local covered
  local total
  local percentage

  counts="$(sed -nE 's/.*expressions used \(([0-9]+)\/([0-9]+)\).*/\1 \2/p' <<<"${report}")"
  read -r covered total <<<"${counts}"

  if [[ -z "${covered:-}" || -z "${total:-}" || "${total}" -eq 0 ]]; then
    echo "${label}: expression coverage could not be measured" >&2
    exit 1
  fi

  percentage="$(awk -v covered="${covered}" -v total="${total}" \
    'BEGIN { printf "%.2f", covered * 100 / total }')"
  echo "${label}: ${percentage}% expressions (${covered}/${total})"

  if ((covered * 100 < total * COVERAGE_THRESHOLD)); then
    echo "${label}: coverage is below ${COVERAGE_THRESHOLD}%" >&2
    exit 1
  fi
}

cd "${REPOSITORY_ROOT}"

cabal test \
  "shared:shared-test" \
  --test-show-details=direct

find "${REPOSITORY_ROOT}/dist-newstyle" \
  -path '*/hpc/vanilla/tix/media-unit-test.tix' \
  -delete

cabal test \
  "media:${UNIT_TEST}" \
  --enable-coverage \
  --disable-shared \
  --enable-static \
  --disable-optimization \
  --test-show-details=direct \
  -j1

unit_test_binary="$(
  cabal list-bin \
    "media:${UNIT_TEST}" \
    --enable-coverage \
    --disable-shared \
    --enable-static \
    --disable-optimization
)"
unit_component_root="${unit_test_binary%/build/"${UNIT_TEST}"/"${UNIT_TEST}"}"
package_root="${unit_component_root%%/t/*}"
component_profile="${unit_component_root#"${package_root}/t/${UNIT_TEST}"}"

CORE_MIX_DIRECTORY="${package_root}${component_profile}/build/"
CORE_MIX_DIRECTORY+="extra-compilation-artifacts/hpc/vanilla/mix"
readonly CORE_MIX_DIRECTORY
CORE_PACKAGE_MIX_DIRECTORY="${CORE_MIX_DIRECTORY}/media-0.1.0.0-inplace"
readonly CORE_PACKAGE_MIX_DIRECTORY
PRESENTATION_MIX_DIRECTORY="${package_root}/l/worker-adapters${component_profile}/"
PRESENTATION_MIX_DIRECTORY+="build/worker-adapters/"
PRESENTATION_MIX_DIRECTORY+="extra-compilation-artifacts/hpc/vanilla/mix"
readonly PRESENTATION_MIX_DIRECTORY
PRESENTATION_PACKAGE_MIX_DIRECTORY="${PRESENTATION_MIX_DIRECTORY}/"
PRESENTATION_PACKAGE_MIX_DIRECTORY+="media-0.1.0.0-inplace-worker-adapters"
readonly PRESENTATION_PACKAGE_MIX_DIRECTORY

unit_tix="${unit_component_root}/hpc/vanilla/tix/${UNIT_TEST}.tix"

domain_report="$(coverage_report "${unit_tix}" "${DOMAIN_MODULES[@]}")"
use_case_report="$(coverage_report "${unit_tix}" "${USE_CASE_MODULES[@]}")"
presentation_report="$(coverage_report "${unit_tix}" "${PRESENTATION_MODULES[@]}")"

assert_expression_coverage "Media domain" "${domain_report}"
assert_expression_coverage "Media use cases" "${use_case_report}"
assert_expression_coverage "Media presentation" "${presentation_report}"
