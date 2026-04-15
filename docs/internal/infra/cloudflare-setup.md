# Cloudflare セットアップ手順書 (hut reader / prd)

本手順書は `hut.lihs.dev` ドメインを Cloudflare で管理し、Reader アプリケーションを OpenNext 経由で Cloudflare Workers にデプロイするための運用者向けドキュメントである。対象環境は本番 (prd) のみで、ステージング (stg) は既存の Cloud Run (`hut-stg-reader`) を継続利用するため本手順の対象外とする。

## 目次

1. [前提条件](#1-前提条件)
2. [Cloudflare アカウント準備と DNS 委譲](#2-cloudflare-アカウント準備と-dns-委譲)
3. [Cloudflare Workers Custom Domain 紐付け](#3-cloudflare-workers-custom-domain-紐付け)
4. [必要な認証情報 (API Token / Account ID / Zone ID)](#4-必要な認証情報-api-token--account-id--zone-id)
5. [最終的な DNS レコードの期待値](#5-最終的な-dns-レコードの期待値)
6. [セキュリティ設定 (Bot Fight Mode / Security Level ほか)](#6-セキュリティ設定-bot-fight-mode--security-level-ほか)
7. [Rate Limit Rules 設定](#7-rate-limit-rules-設定)
8. [Cache Rules 設定](#8-cache-rules-設定)
9. [Purge (キャッシュパージ) 手順](#9-purge-キャッシュパージ-手順)
10. [ロールバック手順](#10-ロールバック手順)

---

## 1. 前提条件

- Cloudflare のアカウントを所有していること。未取得の場合は [https://dash.cloudflare.com/sign-up](https://dash.cloudflare.com/sign-up) から Free Plan を開設する。
- `lihs.dev` のレジストラ (ドメイン登録業者) の管理画面にアクセスできる権限を持っていること。
- GitHub Organization `lihs-ie` の admin 権限を持っていること (Secrets 登録に必要)。
- `wrangler` CLI をローカルで使用する場合はインストール済みであること。未インストールの場合は `pnpm add -g wrangler` あるいは `npm i -g wrangler` で入れる。
- 本番用 Worker 名は `hut-prd-reader` とする。
- OpenNext のビルド成果物は `applications/frontend/reader/.open-next/` 以下に出力される想定。
- 認証情報の投入は `docs/internal/infra/cloudflare-github-secrets.md` の手順を完了している (GitHub Secrets / Variables、Wrangler Secret / Vars 両方)。
- `applications/frontend/reader/wrangler.toml` の `[vars]` セクションのうち `NEXT_PUBLIC_FIREBASE_PROJECT_ID` / `NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET` は本番値で埋められている (空文字のままだと Worker が起動時にエラー)。

---

## 2. Cloudflare アカウント準備と DNS 委譲

### 2.1. Zone の作成

1. Cloudflare Dashboard ([https://dash.cloudflare.com/](https://dash.cloudflare.com/)) にログインする。
2. 左ペインの `Websites` を選択し、右上の `Add a site` をクリック。
3. 入力欄に `lihs.dev` を入力して `Continue` を押下。
   - 注意: apex である `lihs.dev` を Zone として登録する。`hut.lihs.dev` はそのサブドメインとして扱う。もし既に `lihs.dev` が Cloudflare に登録済みの場合は新規登録ではなく既存 Zone を使用する。
4. プラン選択画面で `Free` を選び `Continue`。
5. Cloudflare が既存の DNS レコードを自動スキャンする。スキャン結果に他のサブドメインが既に存在する場合はその設定を変更しないことを確認する。
6. `Continue` を押下して DNS レコード確認ステップを完了する。

### 2.2. Name Server の確認

1. Zone 作成直後、Cloudflare は 2 つの Name Server を割り当てる (例: `xxx.ns.cloudflare.com`, `yyy.ns.cloudflare.com`)。
2. Overview 画面 (`https://dash.cloudflare.com/<account-id>/lihs.dev`) の `Cloudflare Nameservers` セクションに表示される値をメモする。
3. Cloudflare から払い出された 2 つの Name Server 名は固定であり変更されない (Cloudflare が明示的に変更しない限り)。

### 2.3. レジストラ側での Name Server 変更

レジストラ (例: お名前.com / Google Domains / Squarespace Domains / Namecheap など) の管理画面から、`lihs.dev` の Name Server を Cloudflare の 2 つに変更する。

手順の例 (代表的なレジストラでの流れ):

- `Domain Management` または `ドメイン設定` メニューを開く
- `lihs.dev` を選択
- `Name Server` または `ネームサーバー設定` の項目を選択
- `Use custom name servers` を選び、Cloudflare から払い出された 2 つの Name Server を入力
- 保存 (反映にはレジストラによって数分 - 数時間かかる)

**注意**:
- 既存の Name Server 設定は削除 (上書き) してよいが、作業前に必ず元の Name Server 値をメモ (ロールバック用)。
- `lihs.dev` に他のサブドメインで運用中のサービスがある場合、Cloudflare 側の DNS レコードに事前に同じ内容を登録しておかないと、委譲後にそれらのサービスが停止する。本手順では `hut.lihs.dev` の prd 切り替えが主目的だが、他サブドメインがある場合は先に DNS レコードを揃えること。

### 2.4. Propagation (伝播) の確認

1. レジストラ側で Name Server を変更後、最大 48 時間の伝播期間が発生する (通常は数分から数時間で反映)。
2. 伝播状況は以下のコマンドで確認できる:
   ```bash
   dig NS lihs.dev +short
   ```
   Cloudflare の Name Server が返ってくれば伝播完了。
3. Cloudflare Dashboard の Zone Overview には `Active` ステータスが表示される。`Pending Nameserver Update` の間は引き続き旧 Name Server が有効。
4. 伝播完了までの間、Cloudflare 経由のトラフィックは流れないため、本番切り替えは必ず `Active` 状態を確認してから行う。

### 2.5. apex (`lihs.dev`) には本手順では触れない

本手順の目的は `hut.lihs.dev` のみを Cloudflare Workers に向けることであり、apex `lihs.dev` の既存用途 (他サービス運用中であればそれ) は変更しない。Zone 登録時にスキャンされた既存レコードは保持する。

---

## 3. Cloudflare Workers Custom Domain 紐付け

Reader Worker (`hut-prd-reader`) を `hut.lihs.dev` に紐付ける手順。

### 3.1. Worker デプロイ完了の確認

1. 事前に `docs/internal/infra/cloudflare-github-secrets.md` の手順に従い、GitHub Secrets / Variables (CLOUDFLARE_API_TOKEN / CLOUDFLARE_ACCOUNT_ID) と Wrangler Secret (FIREBASE_SERVICE_ACCOUNT_EMAIL / FIREBASE_SERVICE_ACCOUNT_PRIVATE_KEY) を投入済みであること。
2. GitHub Actions の `Deploy to Cloudflare Workers` ワークフロー (`.github/workflows/deploy-cloudflare.yml`) を以下のいずれかで起動する:
   - 直接起動: `Actions` タブ → `Deploy to Cloudflare Workers` → `Run workflow` → `environment=prd` を選択。
   - 既存の `Deploy to Cloud Run` (`deploy.yml`) から `application=reader` / `environment=prd` を選んだ場合、`validate` ジョブが自動で `deploy-cloudflare.yml` を `workflow_call` で呼び出す (`deploy.yml` の `deploy-cloudflare` ジョブ)。
3. デプロイ完了後、Cloudflare Dashboard の `Workers & Pages` から `hut-prd-reader` が存在することを確認する。
4. デフォルトの `*.workers.dev` サブドメイン (例: `hut-prd-reader.<account>.workers.dev`) でアクセスできることを確認する。
   - ただし Reader Worker は production で `CF-Connecting-IP` ヘッダ (Cloudflare 経由のトラフィックにのみ付与される) を見て直接アクセスを遮断するガードを有効化している (`applications/frontend/reader/src/proxy.ts`)。`*.workers.dev` 経由でも Cloudflare Edge を通るためヘッダは付与されるが、本番運用時は必ず Custom Domain (`hut.lihs.dev`) 経由で確認すること。

### 3.2. Custom Domain の追加

1. Cloudflare Dashboard → `Workers & Pages` → `hut-prd-reader` を開く。
2. `Settings` タブ → `Triggers` セクションに移動。
3. `Custom Domains` の `Add Custom Domain` をクリック。
4. `Domain` 欄に `hut.lihs.dev` を入力。
5. `Add Custom Domain` を押下すると Cloudflare が自動的に以下を行う:
   - `lihs.dev` Zone に `hut` の CNAME (あるいは直接の Worker route) を自動作成する
   - TLS 証明書を自動発行する (Universal SSL)
6. 数分待機し、`Status` が `Active` になることを確認する。
7. `https://hut.lihs.dev` にアクセスし、Worker のレスポンスが返ることを確認する。

### 3.3. Proxy (Orange Cloud) の有効化

Custom Domain として紐付けた場合、Cloudflare が自動的にその DNS レコードを Proxy モード (Orange Cloud) で作成する。確認手順:

1. `lihs.dev` の Zone に移動。
2. `DNS` → `Records` を開く。
3. `hut` の A / AAAA / CNAME レコードが Proxy 状態 (オレンジの雲アイコン) になっていることを確認する。
4. Proxy が無効 (灰色の雲) になっていた場合はアイコンをクリックして有効化する。

---

## 4. 必要な認証情報 (API Token / Account ID / Zone ID)

GitHub Actions および Wrangler CLI 実行で必要となる認証情報の取得場所は以下の通り。詳細な Secrets 登録手順は `docs/internal/infra/cloudflare-github-secrets.md` を参照。

### 4.1. Account ID

1. Cloudflare Dashboard の任意の Zone (例: `lihs.dev`) を開く。
2. 右サイドバーの `API` セクション → `Account ID` の値をコピー。
3. 16 進数 32 文字の文字列。

### 4.2. Zone ID

1. `lihs.dev` Zone の Overview を開く。
2. 右サイドバーの `API` セクション → `Zone ID` の値をコピー。
3. 16 進数 32 文字の文字列。Rate Limit / Cache Rule を curl や Terraform から設定する際に必要。

### 4.3. API Token

1. Cloudflare Dashboard 右上のユーザーアイコン → `My Profile` → `API Tokens` タブ。
2. `Create Token` を押下。
3. テンプレートではなく `Create Custom Token` を選ぶ。
4. 権限設定 (最小権限で作成する):
   - `Account` - `Workers Scripts` - `Edit`
   - `Account` - `Account Settings` - `Read`
   - `Zone` - `Cache Purge` - `Purge`
   - `Zone` - `Workers Routes` - `Edit`
5. `Account Resources`: `Include` - 対象アカウントのみ
6. `Zone Resources`: `Include` - `Specific zone` - `lihs.dev`
7. `TTL`: 必要に応じて期限を設定 (デフォルトは無期限)
8. `Continue to summary` → `Create Token`
9. 表示された Token を控える (再表示不可)。控えを安全に保管してから `Continue` を押下。

---

## 5. 最終的な DNS レコードの期待値

Custom Domain 紐付け完了後、`lihs.dev` Zone の DNS レコードは以下の状態になる。

| タイプ | 名前 | 値 | Proxy | 備考 |
|--------|------|---|-------|------|
| A / AAAA / CNAME (自動) | `hut` | Cloudflare Workers 管理 | Proxied (Orange Cloud) | Custom Domain によって自動生成される |
| 既存 | 他サブドメイン | (既存のまま変更なし) | (既存のまま) | 本手順では触らない |

Custom Domain 機能は内部的に `hut` を Worker ルートとして扱うため、`wrangler.toml` の `routes` セクションでも同じ値 (`hut.lihs.dev/*`) を宣言する。UI 上は A/AAAA レコードとして表示されることがある (Cloudflare 側の実装依存)。

---

## 6. セキュリティ設定 (Bot Fight Mode / Security Level ほか)

Free Plan の範囲で設定可能なセキュリティ項目を有効化する。Free Plan で利用不可な項目は該当ステップで明示的にスキップする。

### 6.1. Bot Fight Mode

1. Cloudflare Dashboard → `lihs.dev` Zone → `Security` → `Bots`。
2. `Bot Fight Mode` を `On` に切り替える。
3. 高度な `Super Bot Fight Mode` は Pro Plan 以上のため Free Plan ではスキップする。

### 6.2. Security Level

1. `Security` → `Settings` (旧 WAF → Tools → Security Level)。
2. `Security Level` を `Medium` に設定する。
3. 目的: 過去 14 日間の脅威スコアが中程度以上の IP に対してチャレンジを発行。

### 6.3. Challenge Passage

1. `Security` → `Settings` → `Challenge Passage`。
2. 推奨値: `30 minutes` (デフォルトは 30 分)。
3. 特定リージョン (Asia) の正当トラフィックがチャレンジを過度に受けていないかはアクセスログで監視し、必要に応じて延長。

### 6.4. Browser Integrity Check

1. `Security` → `Settings` → `Browser Integrity Check`。
2. `On` に切り替える。
3. ヘッダ偽装や User-Agent 偽装の抑止。

### 6.5. Scrape Shield (Email Obfuscation / Hotlink Protection)

1. `Scrape Shield` メニューを開く (`Settings` → `Scrape Shield` もしくはサイドバーの `Scrape Shield`)。
2. `Email Address Obfuscation` を `On`。
3. `Hotlink Protection` を `On`。画像の外部埋め込みを抑止。
   - ただし Reader では Firebase Storage の public URL を直接参照するため、`Hotlink Protection` は Cloudflare 側の配信ドメインにしか影響しない点に注意。

### 6.6. Free Plan で利用不可な項目 (スキップ)

以下は Pro Plan 以上でのみ利用可能なためスキップする:

- `WAF Custom Rules` (Pro 以上)
- `Managed Rules` (Pro 以上)
- `Super Bot Fight Mode` (Pro 以上)
- `Rate Limiting Rules` の 2 ルール目以降 (Free Plan は 1 ルールのみ利用可能)
- `Page Shield` (Pro 以上)

---

## 7. Rate Limit Rules 設定

Free Plan は Rate Limit Rule を **1 つだけ** 作成できる。Reader の Abuse 対策として最重要となる `/search` エンドポイントに適用する。

### 7.1. 設定値 (決定版)

- **対象パス**: `/search`
- **制限**: 30 requests / 60 seconds per IP
- **Action**: Managed Challenge
- **Mitigation Timeout**: 600 秒 (10 分)
- **優先度**: Free Plan では 1 ルールのみなので優先度は考慮不要

`/api/*` についても将来的に Rate Limit を追加したい場合は Pro Plan へのアップグレードを検討する。本 PR 時点では MVP として `/search` のみ保護する。

### 7.2. Dashboard からの設定手順

1. Cloudflare Dashboard → `lihs.dev` Zone → `Security` → `WAF` → `Rate limiting rules` タブ。
2. `Create rule` をクリック。
3. `Rule name`: `reader search rate limit`
4. `If incoming requests match`:
   - `Field`: `URI Path`
   - `Operator`: `equals`
   - `Value`: `/search`
5. `Then rate limit by`:
   - `Requests per`: `60 seconds`
   - `Requests per counting expression`: `30`
6. `Counting characteristics`:
   - `IP source address` を選択
7. `With the following action`:
   - `Managed Challenge` を選択
8. `Duration`: `600 seconds`
9. `Deploy` をクリック。

### 7.3. JSON インポート (参考)

Dashboard UI からの設定を推奨するが、API 経由で適用する場合のリクエストボディのスキーマは `docs/internal/infra/cloudflare-rate-limit.json` を参照。curl での適用例:

```bash
CF_API_TOKEN=xxx
CF_ZONE_ID=xxx

curl -X POST "https://api.cloudflare.com/client/v4/zones/${CF_ZONE_ID}/rulesets" \
  -H "Authorization: Bearer ${CF_API_TOKEN}" \
  -H "Content-Type: application/json" \
  --data @docs/internal/infra/cloudflare-rate-limit.json
```

注意: Rulesets API を使った操作は Dashboard 経由より複雑で、既存の Rulesets との整合性取りが必要になる。運用初期は Dashboard からの手動設定を推奨する。

### 7.4. Wrangler 経由の適用

Wrangler CLI は Rate Limit Rule を直接管理する機能を持たないため、Wrangler 経由での適用は現時点では非対応。Dashboard または REST API で設定する。

### 7.5. 動作確認

設定後、`/search` に対して 60 秒間に 31 回以上のリクエストを送ると Managed Challenge が発火する。curl での簡易確認:

```bash
for i in $(seq 1 35); do
  curl -s -o /dev/null -w "%{http_code}\n" "https://hut.lihs.dev/search?q=test"
done
```

31 回目以降が `403` または Challenge ページに相当するレスポンスになることを確認する。

---

## 8. Cache Rules 設定

Reader は静的 (ISR / SSG に近い性質) なコンテンツが中心であり、Cloudflare CDN でのキャッシュを積極活用する。

### 8.1. TTL 決定値 (Frontend B-05 と共有)

Frontend 担当は next.config / OpenNext 設定上の `Cache-Control` ヘッダを以下の TTL に合わせる。

| パス | Edge TTL | Browser TTL | SWR (stale-while-revalidate) | 備考 |
|------|----------|-------------|------------------------------|------|
| `/articles/*` | 86400 (1 day) | 300 (5 min) | 604800 (7 days) | 本文記事 |
| `/memos/*` | 86400 (1 day) | 300 (5 min) | 604800 (7 days) | メモ |
| `/series/*` | 86400 (1 day) | 300 (5 min) | 604800 (7 days) | シリーズ記事 |
| `/feed.xml` | 3600 (1 hour) | 600 (10 min) | - | RSS |
| `/sitemap.xml` | 3600 (1 hour) | 600 (10 min) | - | サイトマップ |

**TTL 決定の根拠**:
- 本文コンテンツは更新頻度が日次以下のため Edge TTL 1 day で問題なし。
- Browser TTL 5 min は再訪問時の体感速度を維持しつつ、編集直後の反映遅延を許容範囲に収めるため。
- SWR 7 days は Cloudflare の stale-while-revalidate を活用し、オリジンに負荷をかけずに古いレスポンスを即返しつつ裏で再生成する戦略。
- フィード系は RSS リーダーの更新頻度を考慮し 1 時間程度が適切。

### 8.2. Origin Cache-Control の扱い方針

**方針**: Origin (OpenNext Worker) の `Cache-Control` ヘッダを Cloudflare 側で尊重する。

理由:
- Frontend 側の `Cache-Control` を変更すればデプロイだけで TTL を調整可能。
- Cloudflare 側の設定と Frontend 側のヘッダが二重管理になるのを避ける。
- OpenNext は Next.js の `revalidate` 指定から適切な Cache-Control を生成するため整合性が取りやすい。

### 8.3. Dashboard からの Cache Rule 作成手順

以下は `/articles/*` の例。`/memos/*`, `/series/*`, `/feed.xml`, `/sitemap.xml` も同様の手順で作成する。

1. Cloudflare Dashboard → `lihs.dev` Zone → `Caching` → `Cache Rules` タブ。
2. `Create rule` をクリック。
3. `Rule name`: `reader articles cache`
4. `If incoming requests match`:
   - `Field`: `URI Path`
   - `Operator`: `wildcard`
   - `Value`: `/articles/*`
5. `Then`:
   - `Cache eligibility`: `Eligible for cache`
   - `Edge TTL`: `Use cache-control header if present, bypass cache if not present`
     - (Origin 尊重方針。ヘッダが無ければキャッシュしない)
   - `Browser TTL`: `Respect origin TTL`
6. `Cache Key`:
   - `Query strings`: `Ignore query string`
   - `Cookie`: (チェックなし)
   - `Host header`: デフォルト (include origin host)
7. `Deploy` をクリック。

**重要**: `/feed.xml` と `/sitemap.xml` は個別ルールで作成する (`wildcard` マッチではなく `equals` で path を指定)。

### 8.4. Cache Rule 全件の期待値

| Rule Name | Match | Edge TTL | Browser TTL | Query Strings | 備考 |
|-----------|-------|----------|-------------|---------------|------|
| `reader articles cache` | `/articles/*` | Use Cache-Control | Respect Origin | Ignore | SWR は Origin ヘッダで |
| `reader memos cache` | `/memos/*` | Use Cache-Control | Respect Origin | Ignore | |
| `reader series cache` | `/series/*` | Use Cache-Control | Respect Origin | Ignore | |
| `reader feed cache` | `/feed.xml` | Use Cache-Control | Respect Origin | Ignore | `equals` で指定 |
| `reader sitemap cache` | `/sitemap.xml` | Use Cache-Control | Respect Origin | Ignore | `equals` で指定 |

### 8.5. Frontend 側で設定すべき Cache-Control ヘッダ (参考)

Frontend 担当の参考値として以下を提示する。実装は Frontend 側で行う。

```
# /articles/*, /memos/*, /series/*
Cache-Control: public, max-age=300, s-maxage=86400, stale-while-revalidate=604800

# /feed.xml, /sitemap.xml
Cache-Control: public, max-age=600, s-maxage=3600
```

`max-age` が Browser TTL、`s-maxage` が Edge TTL、`stale-while-revalidate` が SWR 相当。

---

## 9. Purge (キャッシュパージ) 手順

デプロイ後あるいは緊急でキャッシュを無効化する場合の手順。

### 9.1. Dashboard からの Purge

1. `lihs.dev` Zone → `Caching` → `Configuration` タブ。
2. `Purge Cache` セクション:
   - `Purge Everything`: 全パージ (緊急時のみ推奨)
   - `Custom Purge`: 特定 URL / Host / Tag 指定
3. Custom Purge の場合、`URL` を選択し対象 URL を 1 行ずつ入力 (例: `https://hut.lihs.dev/articles/xxx`)。
4. `Purge` をクリック。

### 9.2. CLI からの Purge (wrangler)

```bash
# 特定 URL のパージ
wrangler purge --urls "https://hut.lihs.dev/articles/xxx"

# 全パージ
wrangler purge --all
```

注意: `wrangler purge` コマンドは Wrangler のバージョンによって利用できないケースがある。利用可否はリリース時の Wrangler バージョンで確認する。利用不可の場合は以下の curl 方式を使用する。

### 9.3. curl による REST API Purge

```bash
CF_API_TOKEN=xxx
CF_ZONE_ID=xxx

# 特定 URL
curl -X POST "https://api.cloudflare.com/client/v4/zones/${CF_ZONE_ID}/purge_cache" \
  -H "Authorization: Bearer ${CF_API_TOKEN}" \
  -H "Content-Type: application/json" \
  --data '{"files":["https://hut.lihs.dev/articles/xxx"]}'

# 全パージ
curl -X POST "https://api.cloudflare.com/client/v4/zones/${CF_ZONE_ID}/purge_cache" \
  -H "Authorization: Bearer ${CF_API_TOKEN}" \
  -H "Content-Type: application/json" \
  --data '{"purge_everything":true}'
```

### 9.4. Purge 後の検証

```bash
# Cache Hit / Miss の確認
curl -I https://hut.lihs.dev/articles/xxx | grep -i cf-cache-status
```

1 回目は `MISS`、2 回目以降は `HIT` となることを確認。

---

## 10. ロールバック手順

万一 Cloudflare Workers デプロイ後に問題が発生した場合の復旧手順。

### 10.1. Worker バージョンのロールバック

1. Cloudflare Dashboard → `Workers & Pages` → `hut-prd-reader` → `Deployments` タブ。
2. 過去のデプロイ履歴から正常だったバージョンを選択。
3. `Rollback to this deployment` をクリック。
4. 確認ダイアログで `Rollback` を承認。

CLI での実行:

```bash
# 過去のデプロイ履歴を確認
wrangler deployments list

# 特定のデプロイ ID にロールバック
wrangler rollback <deployment-id>
```

### 10.2. Custom Domain の切り戻し

Cloudflare Workers 経由が完全に機能しない場合、`hut.lihs.dev` を Cloud Run (STG の Reader) に一時的に向ける選択肢も検討する (ただし STG を本番トラフィックにさらすのは非推奨で、あくまで緊急回避手段)。

1. Cloudflare Dashboard → `lihs.dev` Zone → `DNS` → `Records`。
2. `hut` の既存 Custom Domain レコードを無効化 (Proxy を Off にするのではなく、Worker 紐付けを外す)。
3. `Workers & Pages` → `hut-prd-reader` → `Settings` → `Triggers` → `Custom Domains` から `hut.lihs.dev` を削除。
4. `DNS` → `Records` で `hut` の CNAME を Cloud Run の STG エンドポイント (`hut-stg-reader-xxx.a.run.app`) に変更 (Proxy は Off 推奨、Cloud Run 側の TLS を使うため)。

注意: この操作は緊急時のみ。恒常的な構成としては STG と PRD のコードが同一保証されないため避ける。

### 10.3. DNS Name Server のロールバック (最終手段)

`lihs.dev` の Name Server 自体を Cloudflare 委譲前の値に戻す手順。

1. レジストラ管理画面にログイン。
2. `lihs.dev` の Name Server 設定を、本手順 2.3 でメモした委譲前の値に戻す。
3. 伝播に最大 48 時間かかる。
4. Cloudflare 経由のトラフィックが止まり、元の DNS 運用に戻る。

この操作は Cloudflare 上で構築した Rate Limit / Cache Rule / Workers の設定も事実上無効になる。実施は最終手段として、Cloudflare 側の不具合が Worker ロールバックで解決できない場合にのみ検討する。

### 10.4. Rate Limit / Cache Rule の一時無効化

個別の Rule が誤検知を起こしている場合の対処:

1. `Security` → `WAF` → `Rate limiting rules` から該当 Rule を選び `Disable`。
2. `Caching` → `Cache Rules` から該当 Rule を選び `Disable`。

完全削除ではなく一時無効化とすることで、原因特定後の再有効化を容易にする。

---

## 付録: 関連ドキュメント

- [cloudflare-github-secrets.md](./cloudflare-github-secrets.md) — GitHub Secrets / Wrangler Secret 登録と API Token ローテーション手順
- [cloudflare-rate-limit.json](./cloudflare-rate-limit.json) — Rate Limit Rule の JSON 定義 (参考)
- `.github/workflows/deploy-cloudflare.yml` — Workers デプロイ用 GitHub Actions workflow (本手順書と連動)
- `applications/frontend/reader/wrangler.toml` — Reader Worker の Wrangler 設定
