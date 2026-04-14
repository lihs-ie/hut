#!/usr/bin/env bash
#
# create-plan-environments.sh
#
# Terraform CI plan job 専用の GitHub Environment (stg-plan / prd-plan) を作成し、
# 既存の stg / prd environment から 6 個の必須 secret をコピーするための対話スクリプト。
#
# 背景:
#   terraform-ci.yml の plan job は PR トリガで environment を参照しているため、
#   既存 stg / prd environment に deployment_branch_policy を設定すると
#   feature branch の PR で CI が壊れる。
#
#   対策として plan 専用環境 (stg-plan / prd-plan) を作成し、
#   terraform-ci.yml はそちらを参照、既存 stg / prd は deploy/migrate 専用として
#   deployment_branch_policy (main/staging 限定) を適用する。
#
# 前提:
#   - gh CLI 認証済み (gh auth status)
#   - 既存 stg / prd environment の 6 secret の値を手元に用意していること
#     (GitHub API では secret の値を取得できないため)
#
# 使い方:
#   bash scripts/create-plan-environments.sh
#
# 実行すると:
#   1. stg-plan / prd-plan environment を作成
#   2. 各 plan 環境に 6 secret をコピー (対話入力)
#   3. 最後に作成された secret 名の一覧を表示して検証
#

set -euo pipefail

REPO="lihs-ie/hut"
SOURCE_ENVS=("stg" "prd")
PLAN_ENVS=("stg-plan" "prd-plan")

# terraform-ci.yml の plan job が参照する 6 個の secret のみコピー
# Firebase 系 / SERVER_ACTIONS_ALLOWED_ORIGINS は deploy.yml 専用なので除外
SECRET_NAMES=(
  "GCP_PROJECT_ID"
  "GCP_SERVICE_ACCOUNT_EMAIL"
  "GCP_WORKLOAD_IDENTITY_PROVIDER"
  "TF_VAR_AUTHORIZED_MEMBERS"
  "TF_VAR_OAUTH_CLIENT_ID"
  "TF_VAR_OAUTH_CLIENT_SECRET"
)

echo "=== GitHub CLI auth check ==="
gh auth status
echo ""

echo "=== 1. Create plan-only environments ==="
for ENV in "${PLAN_ENVS[@]}"; do
  echo "Creating/updating environment: ${ENV}"
  gh api -X PUT "repos/${REPO}/environments/${ENV}" >/dev/null
  echo "  -> ok"
done
echo ""

echo "=== 2. Copy secrets into plan environments ==="
echo "You will be prompted 12 times in total (6 secrets x 2 environments)."
echo "Paste the existing secret value from stg/prd when asked."
echo "Tip: gh secret set reads the value from stdin; press Enter after pasting and Ctrl-D to finalize."
echo ""

for i in 0 1; do
  SRC="${SOURCE_ENVS[$i]}"
  DST="${PLAN_ENVS[$i]}"
  echo "--- Source: ${SRC} -> Destination: ${DST} ---"
  for NAME in "${SECRET_NAMES[@]}"; do
    echo ""
    echo "Enter value for ${NAME} (from ${SRC}):"
    gh secret set "${NAME}" --env "${DST}" --repo "${REPO}"
  done
  echo ""
done

echo ""
echo "=== 3. Verification ==="
for ENV in "${PLAN_ENVS[@]}"; do
  echo "Secrets in ${ENV}:"
  gh api "repos/${REPO}/environments/${ENV}/secrets" --jq '.secrets[].name' | sed 's/^/  - /'
done

echo ""
echo "=== Done ==="
echo ""
echo "Next steps:"
echo "  1. Merge PR that switches terraform-ci.yml matrix to stg-plan / prd-plan."
echo "  2. Apply deployment_branch_policy on the existing stg / prd environments"
echo "     (prd -> main only, stg -> staging only)."
echo "     This is handled by Claude post-merge."
