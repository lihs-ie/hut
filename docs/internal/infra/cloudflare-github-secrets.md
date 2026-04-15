# Cloudflare 関連 GitHub Secrets 登録手順書

`.github/workflows/deploy-cloudflare.yml` から Cloudflare Workers へデプロイするために必要な認証情報を GitHub Secrets / Variables に登録し、あわせて Reader Worker が実行時に必要とする Firestore サービスアカウント資格情報 (client_email / private_key) および Firebase プロジェクト情報を Wrangler Secret / Vars として投入する手順。

## 目次

1. [登録対象の一覧](#1-登録対象の一覧)
2. [Cloudflare API Token の作成](#2-cloudflare-api-token-の作成)
3. [Cloudflare Account ID / Zone ID の取得](#3-cloudflare-account-id--zone-id-の取得)
4. [Firestore サービスアカウント鍵の準備](#4-firestore-サービスアカウント鍵の準備)
5. [GitHub Secrets / Variables への登録](#5-github-secrets--variables-への登録)
6. [Wrangler Secret / Vars への投入](#6-wrangler-secret--vars-への投入)
7. [Token のローテーション手順](#7-token-のローテーション手順)
8. [検証手順](#8-検証手順)

---

## 1. 登録対象の一覧

| 名前 | 種別 | 用途 | 保存場所 |
|------|------|------|----------|
| `CLOUDFLARE_API_TOKEN` | Repository Secret | wrangler-action が Cloudflare へ認証するため | GitHub Secrets |
| `CLOUDFLARE_ACCOUNT_ID` | Repository Variable | Worker のデプロイ先 Account を指定 | GitHub Variables |
| `FIREBASE_SERVICE_ACCOUNT_EMAIL` | Wrangler Secret (Worker 実行時) | Firestore REST API 認証時の client_email | Cloudflare Workers の secret store |
| `FIREBASE_SERVICE_ACCOUNT_PRIVATE_KEY` | Wrangler Secret (Worker 実行時) | Firestore REST API 認証時の private_key (PEM) | Cloudflare Workers の secret store |
| `NEXT_PUBLIC_FIREBASE_PROJECT_ID` | Wrangler Var (Worker 実行時) | Firestore プロジェクト ID | `applications/frontend/reader/wrangler.toml` の `[vars]` |
| `NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET` | Wrangler Var (Worker 実行時) | Firebase Storage バケット (画像配信参照) | `applications/frontend/reader/wrangler.toml` の `[vars]` |

Reader Worker (`applications/frontend/reader/src/providers/infrastructure/firestore.ts`) の実装は `FIREBASE_SERVICE_ACCOUNT_EMAIL` と `FIREBASE_SERVICE_ACCOUNT_PRIVATE_KEY` を **別々の環境変数** として読むため、サービスアカウント JSON をそのまま Secret として投入することはできない。JSON から 2 つの値を抽出して個別に投入する (詳細は手順 6)。

`FIREBASE_SERVICE_ACCOUNT_*` は GitHub Secrets に保存するのではなく、運用者が `wrangler secret put` で直接 Cloudflare Workers に投入する。理由:

- Worker の runtime secret であり、ビルドやデプロイ時点では不要。
- GitHub Actions 経由で設定するとログ漏洩リスクがあるため、Cloudflare のエンドツーエンドの Secret 機能を使う。
- Workers 側の Secret は暗号化保存され、dashboard 上でも値は表示されない。

---

## 2. Cloudflare API Token の作成

### 2.1. 最小権限の方針

GitHub Actions から実行される API Token は、必要最小限の権限のみを付与する。

必要権限:

| スコープ | リソース | 権限 | 用途 |
|----------|----------|------|------|
| Account | `Workers Scripts` | `Edit` | `wrangler deploy` の実行 |
| Account | `Account Settings` | `Read` | wrangler が Account 情報を取得 |
| Zone | `Cache Purge` | `Purge` | デプロイ後のキャッシュクリア |
| Zone | `Workers Routes` | `Edit` | Custom Domain / Route の自動設定 |

`DNS:Edit` や `Zone:Edit` 等の広い権限は付与しない。

### 2.2. Dashboard からの作成手順

1. Cloudflare Dashboard 右上のユーザーアイコン → `My Profile` を開く。
2. 左サイドバー `API Tokens` タブを選択。
3. `Create Token` をクリック。
4. `Create Custom Token` の `Get started` を選択 (テンプレートは使用しない)。
5. `Token name`: `hut-github-actions-deploy`
6. `Permissions` で 4 つの権限を追加 (上表参照):
   - 1 行目: `Account` / `Workers Scripts` / `Edit`
   - 2 行目: `Account` / `Account Settings` / `Read`
   - 3 行目: `Zone` / `Cache Purge` / `Purge`
   - 4 行目: `Zone` / `Workers Routes` / `Edit`
7. `Account Resources`:
   - `Include` / 対象 Cloudflare Account を選択
8. `Zone Resources`:
   - `Include` / `Specific zone` / `lihs.dev`
9. `Client IP Address Filtering`: 空欄 (GitHub Actions の IP が変動するため指定不可)
10. `TTL`: 推奨は 1 年 (`End Date` に翌年の日付を設定)。期限なしも可だが定期ローテーション (年 1 回) を推奨。
11. `Continue to summary` → `Create Token`
12. 表示された Token 文字列をコピーし、以降の手順 5 で GitHub Secrets に登録する。
    - **Token は再表示できない** ため、控えた内容を安全な場所に一時保管する (例: 1Password / Bitwarden 等のパスワードマネージャ)。

### 2.3. Token の動作確認 (オプション)

ローカルマシンで Token の有効性を確認する場合:

```bash
curl -s -X GET "https://api.cloudflare.com/client/v4/user/tokens/verify" \
  -H "Authorization: Bearer <TOKEN>" | jq .
```

`"success": true`, `"status": "active"` が返れば正常。

---

## 3. Cloudflare Account ID / Zone ID の取得

### 3.1. Account ID

1. Cloudflare Dashboard で任意の Zone (例: `lihs.dev`) を開く。
2. 右サイドバーの `API` セクションにある `Account ID` をコピー。
3. 16 進数 32 文字。

### 3.2. Zone ID (参考: 本 PR の GitHub Secrets 登録対象外)

1. `lihs.dev` Zone の Overview を開く。
2. 右サイドバーの `API` セクションにある `Zone ID` をコピー。

Zone ID は `deploy-cloudflare.yml` では直接使用しないが、Rate Limit / Cache Rule を REST API 経由で設定する際や、手動 Cache Purge を行う際に必要。運用者のメモに保管する。

---

## 4. Firestore サービスアカウント鍵の準備

Reader Worker は Firestore REST API を service account 認証で直叩きする。本 Worker 専用の SA を新規作成するか、Cloud Run 用の SA を流用するかの判断。

### 4.1. 判断基準

| 観点 | 専用 SA 新規作成 | Cloud Run 用 SA の流用 |
|------|------------------|------------------------|
| 権限分離 | 可能 (Reader の読み取り権限のみに限定) | 既存 SA が持つ権限すべてを継承 |
| 監査ログ | Reader Worker のアクセスが識別可能 | 識別不可 (同一 SA で Cloud Run と Worker が混在) |
| 運用コスト | SA 作成と権限付与の初期作業が必要 | 作業不要 |
| ローテーション | 独立してローテ可能 | 影響範囲が広く慎重に |

**推奨**: 本番運用開始時は **専用 SA を新規作成** する。理由:

- 権限が Firestore の読み取り (`roles/datastore.viewer`) のみに限定できる。
- 監査ログで Worker 経由のアクセスが識別でき、トラブル時の切り分けが容易。
- ローテーション時に Cloud Run 側 (`hut-stg-reader`) に影響しない。

MVP 初期 (本 PR の直後) は時間制約で Cloud Run 用 SA の流用でも可だが、本番トラフィックを流す前に専用 SA への切り替えを行うこと。

### 4.2. 専用 SA の作成手順

1. Google Cloud Console → `IAM & Admin` → `Service Accounts` → `Create Service Account`。
2. `Service account name`: `hut-prd-reader-worker`
3. `Service account ID`: 自動生成される値をそのまま使用
4. `Create and Continue`
5. `Grant this service account access to project` で以下のロールを付与:
   - `Cloud Datastore User` (`roles/datastore.user`) — Firestore の読み取り権限
   - (書き込みが不要なら `Cloud Datastore Viewer` / `roles/datastore.viewer` にしてさらに権限を絞る)
6. `Continue` → `Done`
7. 作成された SA の詳細を開き `Keys` タブ → `Add Key` → `Create new key` → `JSON` を選択。
8. JSON ファイルがダウンロードされる。このファイルから `client_email` と `private_key` を抽出して次のステップ 6 で Wrangler Secret として投入する。

### 4.3. JSON から必要な値を抽出する

ダウンロードした `key.json` から 2 つのフィールドを取り出す。

```bash
# client_email
jq -r '.client_email' /path/to/key.json
# → hut-prd-reader-worker@<project-id>.iam.gserviceaccount.com

# private_key (PEM 形式の改行 \n を含む文字列のまま取得)
jq -r '.private_key' /path/to/key.json
# → "-----BEGIN PRIVATE KEY-----\n...\n-----END PRIVATE KEY-----\n"
```

**重要**: ダウンロードした JSON ファイルは Wrangler Secret 投入後は削除するか、パスワードマネージャに暗号化して保管する。Git にコミットしてはならない。

---

## 5. GitHub Secrets / Variables への登録

### 5.1. `CLOUDFLARE_API_TOKEN` (Repository Secret)

1. GitHub リポジトリ `lihs-ie/hut` を開く。
2. `Settings` → `Secrets and variables` → `Actions`。
3. `Secrets` タブの `New repository secret` をクリック。
4. `Name`: `CLOUDFLARE_API_TOKEN`
5. `Secret`: 手順 2.2 で控えた Token 文字列を貼り付け
6. `Add secret`

### 5.2. `CLOUDFLARE_ACCOUNT_ID` (Repository Variable)

Account ID は秘匿情報ではないため Variable として登録する。

1. 同じ画面で `Variables` タブに切り替え。
2. `New repository variable` をクリック。
3. `Name`: `CLOUDFLARE_ACCOUNT_ID`
4. `Value`: 手順 3.1 で取得した 32 文字の Account ID
5. `Add variable`

`deploy-cloudflare.yml` からは `${{ vars.CLOUDFLARE_ACCOUNT_ID }}` として参照する。

### 5.3. Environment 単位での上書き (任意)

本 PR では prd 環境のみを対象とするが、将来的に preview 環境を追加する際は `Settings` → `Environments` → `prd` / `preview` のそれぞれに同じキーを登録し、環境ごとに値を切り替えられるようにする。現時点では repository secret / variable のみで十分。

---

## 6. Wrangler Secret / Vars への投入

Reader Worker の実行時に必要な Firestore 資格情報 (2 つの Secret) と Firebase プロジェクト情報 (2 つの Var) を投入する。

### 6.1. 事前準備

- 手順 4 でダウンロードした `hut-prd-reader-worker-*.json` (以下 `key.json`) がローカルにあること。
- Wrangler CLI がインストール済みで、`wrangler login` でログイン済みであること。未ログインの場合は以下を実行:
  ```bash
  wrangler login
  ```
  (ブラウザが開き Cloudflare にログイン → Wrangler からのアクセスを許可する)
- ローカルで Worker リポジトリの `applications/frontend/reader/wrangler.toml` が存在するディレクトリに移動しておくこと (`cd applications/frontend/reader`)。
- `jq` がインストール済みであること (macOS は `brew install jq`)。

### 6.2. Secret の投入 (`FIREBASE_SERVICE_ACCOUNT_EMAIL` / `FIREBASE_SERVICE_ACCOUNT_PRIVATE_KEY`)

`wrangler secret put` は `STDIN` から値を読み取る。シェル履歴に値を残さないために、先頭にスペースを付けて実行するか `HISTCONTROL=ignorespace` を設定する。

```bash
cd applications/frontend/reader

# 1) client_email を投入
 jq -r '.client_email' /path/to/key.json | wrangler secret put FIREBASE_SERVICE_ACCOUNT_EMAIL
# → Success! Uploaded secret FIREBASE_SERVICE_ACCOUNT_EMAIL

# 2) private_key を投入
#    key.json の private_key は改行が \n でエスケープされた文字列であり、
#    jq -r がそれを実際の改行に展開する。Reader Worker 側の normalizePrivateKey は
#    どちらの形式 (エスケープ / 実改行) も受け付けるため、jq -r の結果をそのまま渡せばよい。
 jq -r '.private_key' /path/to/key.json | wrangler secret put FIREBASE_SERVICE_ACCOUNT_PRIVATE_KEY
# → Success! Uploaded secret FIREBASE_SERVICE_ACCOUNT_PRIVATE_KEY
```

どちらも `Success! Uploaded secret ...` と表示されれば完了。値を対話式で手入力する場合は、ダブルクォートなしの生文字列を貼り付ける (private_key は改行を含む複数行の文字列になるので注意)。

### 6.3. Vars の設定 (`NEXT_PUBLIC_FIREBASE_PROJECT_ID` / `NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET`)

プロジェクト ID / Storage バケットは機密ではないため Secret ではなく `wrangler.toml` の `[vars]` に記載する。

1. `applications/frontend/reader/wrangler.toml` を編集。
2. `[vars]` セクションの以下の値を本番環境の値で埋める:
   ```toml
   [vars]
   NEXT_PUBLIC_FIREBASE_PROJECT_ID = "hut-prd"
   NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET = "hut-prd.firebasestorage.app"
   ```
3. 変更をコミットし、次回の `deploy-cloudflare.yml` 実行時に反映する。

> 注意: これらの値は Frontend 担当の PR (`applications/frontend/reader/wrangler.toml` 作成) で既にプレースホルダ (空文字) で用意されている。Infrastructure 担当が本番の値を確定させ、必要に応じて Frontend 担当へ値更新を依頼する。

### 6.4. 投入済み Secret の一覧確認

```bash
cd applications/frontend/reader
wrangler secret list
```

`FIREBASE_SERVICE_ACCOUNT_EMAIL` と `FIREBASE_SERVICE_ACCOUNT_PRIVATE_KEY` の 2 つが `Secret` として表示されることを確認する (値は表示されない)。

### 6.5. 投入後の安全対応

- ローカルの `key.json` は削除するか、1Password / Bitwarden に暗号化保管する。
- `wrangler secret put` の入力履歴がシェル履歴に残らないよう、bash の `HISTCONTROL=ignorespace` を活用する (上記コマンドは先頭スペース付きにしてある)。念のため実行後 `history -c` でクリアする。

---

## 7. Token のローテーション手順

定期的 (年 1 回以上) あるいは漏洩が疑われた場合の Token ローテーション手順。

### 7.1. Cloudflare API Token のローテーション

1. 手順 2.2 を再度実行して新しい Token を作成する (名前は `hut-github-actions-deploy-YYYYMMDD` のように日付を付与して判別可能にする)。
2. 作成した新しい Token 文字列を控える。
3. GitHub リポジトリ → `Settings` → `Secrets and variables` → `Actions` → `CLOUDFLARE_API_TOKEN` の `Update` から新しい値に上書きする。
4. `deploy-cloudflare.yml` を一度手動実行し、デプロイが成功することを確認する。
5. 成功確認後、旧 Token を Cloudflare Dashboard の `API Tokens` 一覧から `Delete` する。

ポイント: 新 Token を登録してから旧 Token を削除する。逆の順番で実施すると Actions 実行中に認証エラーになる。

### 7.2. Firestore SA Key のローテーション

1. Google Cloud Console → `IAM` → 対象 SA → `Keys` タブ → `Add Key` → `Create new key` → `JSON` を選択。
2. 新しい `key.json` がダウンロードされる。
3. ローカルで新しい値を 2 つの Secret に上書き投入する (手順 6.2 の 2 つの `wrangler secret put` を再実行)。
   ```bash
   cd applications/frontend/reader
    jq -r '.client_email' /path/to/new-key.json | wrangler secret put FIREBASE_SERVICE_ACCOUNT_EMAIL
    jq -r '.private_key'  /path/to/new-key.json | wrangler secret put FIREBASE_SERVICE_ACCOUNT_PRIVATE_KEY
   ```
   - Wrangler Secret は上書き保存されるため、旧 Secret を削除する必要はない。
4. 次回 Worker リクエスト時から新しい SA Key が使われる (Secret はデプロイ時ではなくランタイムで評価される。ただし Reader Worker 側は access token を内部で短時間キャッシュするため、反映には数分以内のラグがあることに注意)。
5. Cloudflare Workers の動作確認 (任意の `/articles/*` ページが表示されるか確認)。
6. 動作確認後、Google Cloud Console の `Keys` タブから旧 Key を `Delete` する。

### 7.3. ローテーション頻度の目安

| 対象 | 頻度 | トリガー |
|------|------|----------|
| Cloudflare API Token | 年 1 回 | 年次点検あるいは漏洩疑い |
| Firestore SA Key | 90 日ごと | Google Cloud Security Command Center 推奨に準拠 |

---

## 8. 検証手順

Secrets 登録と Wrangler Secret 投入後、実際に `deploy-cloudflare.yml` を実行して検証する。

### 8.1. Workflow Dispatch の実行

1. GitHub リポジトリ `Actions` タブ → `Deploy to Cloudflare Workers` ワークフローを選択。
2. 右上 `Run workflow` ボタン。
3. `Branch`: `staging` (本 PR マージ後)
4. `environment`: `prd`
5. `tag`: 空欄 (git SHA が使われる)
6. `Run workflow` を押下。

### 8.2. ログ確認

- `setup-frontend` composite action の `pnpm install --frozen-lockfile` が成功。
- `pnpm --filter @hut/reader build:worker` が成功し、`applications/frontend/reader/.open-next/` 以下に成果物が出る。
- `cloudflare/wrangler-action` の `command: deploy` が成功し、deployment ID がログに出力される。
- 最終ステップの `wrangler deployments list` で直近のデプロイが表示される。

### 8.3. 実際のアクセス確認

```bash
# 初回アクセス (Cache Miss)
curl -I https://hut.lihs.dev/articles/<sample-slug>

# 2 回目アクセス (Cache Hit 期待)
curl -I https://hut.lihs.dev/articles/<sample-slug> | grep -i cf-cache-status
```

`cf-cache-status: HIT` が返れば Cache Rule と Worker が正常動作している。

### 8.4. Firestore 連携の確認

Reader の任意の記事ページ (`/articles/...`) を開き、Firestore から取得したコンテンツが表示されれば `FIREBASE_SERVICE_ACCOUNT_EMAIL` / `FIREBASE_SERVICE_ACCOUNT_PRIVATE_KEY` Secret が正しく投入されている。失敗する場合は Worker の `tail` ログを確認:

```bash
cd applications/frontend/reader
wrangler tail
```

典型的な失敗メッセージ:

- `Environment variable FIREBASE_SERVICE_ACCOUNT_EMAIL is required.` — Secret 未投入または誤キー。手順 6.2 を再実行。
- `Environment variable FIREBASE_SERVICE_ACCOUNT_PRIVATE_KEY is required.` — 同上。
- `Environment variable NEXT_PUBLIC_FIREBASE_PROJECT_ID is required.` — `wrangler.toml` の `[vars]` でプロジェクト ID が空のまま。手順 6.3 を確認。

---

## 付録: トラブルシューティング

### wrangler-action がエラー `Authentication error [code: 10000]` を返す

- `CLOUDFLARE_API_TOKEN` が失効または権限不足の可能性が高い。
- 手順 2.3 で Token の有効性を確認し、必要に応じて再作成 (手順 7.1)。

### wrangler deploy が `Account ID is required` エラーで失敗する

- `CLOUDFLARE_ACCOUNT_ID` Variable が未登録、または `wrangler.toml` に `account_id` が書かれていない。
- 本 PR では `.github/workflows/deploy-cloudflare.yml` 側で `accountId` を渡しているので、workflow 側の参照が `${{ vars.CLOUDFLARE_ACCOUNT_ID }}` になっていることを確認。

### Worker の runtime で `Environment variable FIREBASE_SERVICE_ACCOUNT_*` エラー

- Wrangler Secret 未投入の可能性。手順 6.4 で Secret 一覧を確認する。
- Secret は登録済みでもエラーが出る場合、`private_key` 内の PEM 改行が欠落している可能性がある (対話式入力で改行を潰してしまったなど)。手順 6.2 の `jq -r` パイプラインでの再投入を推奨。
- Reader Worker の該当コードは `applications/frontend/reader/src/providers/infrastructure/firestore.ts` にある。

### Worker の runtime で `Environment variable NEXT_PUBLIC_FIREBASE_PROJECT_ID` エラー

- `applications/frontend/reader/wrangler.toml` の `[vars]` で該当値が空文字のままになっている可能性。
- 手順 6.3 に従い本番値を記述した上で再デプロイする。
