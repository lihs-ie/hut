#!/usr/bin/env bash
set -euo pipefail

MEDIA_DIR="$(cd "$(dirname "$0")/.." && pwd)"
readonly MEDIA_DIR
readonly PROJECT_FILE="cabal-wasm.project"
readonly BUILD_DIR="${WASM_BUILD_DIR:-${MEDIA_DIR}/dist-newstyle-wasm}"

if ! command -v wasm32-wasi-cabal >/dev/null 2>&1; then
  if [[ -f "${HOME}/.ghc-wasm/env" ]]; then
    # shellcheck disable=SC1091
    source "${HOME}/.ghc-wasm/env"
  elif [[ -f /opt/ghc-wasm/env ]]; then
    # shellcheck disable=SC1091
    source /opt/ghc-wasm/env
  else
    echo "wasm32-wasi-cabal is required" >&2
    exit 127
  fi
fi

build_worker() {
  local executable="$1"
  local runtime_directory="$2"
  local wasm_binary
  local post_linker

  wasm32-wasi-cabal build "exe:${executable}" \
    --project-file="${PROJECT_FILE}" \
    --builddir="${BUILD_DIR}" \
    -j2

  wasm_binary="$(
    wasm32-wasi-cabal list-bin "exe:${executable}" \
      --project-file="${PROJECT_FILE}" \
      --builddir="${BUILD_DIR}" \
      | awk 'NF { line = $0 } END { print line }'
  )"
  if [[ ! -f "${wasm_binary}" ]]; then
    echo "WASM executable was not found: ${wasm_binary}" >&2
    exit 1
  fi
  post_linker="$(wasm32-wasi-ghc --print-libdir)/post-link.mjs"

  "${post_linker}" \
    --input "${wasm_binary}" \
    --output "${runtime_directory}/generated/application-jsffi.mjs"
  printf '\nexport const generatedArtifactKind = "generated";\n' \
    >>"${runtime_directory}/generated/application-jsffi.mjs"
  cp "${wasm_binary}" "${runtime_directory}/generated/application.wasm"
}

cd "${MEDIA_DIR}"

build_worker \
  media-api-worker \
  "${MEDIA_DIR}/runtime/media-api-worker"
build_worker \
  media-inspection-worker \
  "${MEDIA_DIR}/runtime/media-inspection-worker"
build_worker \
  media-reference-projection-worker \
  "${MEDIA_DIR}/runtime/media-reference-projection-worker"
build_worker \
  media-retention-worker \
  "${MEDIA_DIR}/runtime/media-retention-worker"
