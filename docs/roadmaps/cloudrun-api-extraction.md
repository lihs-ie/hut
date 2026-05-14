# Next.js サーバーサイド Firestore 直アクセスを Cloud Run API へ抽出するロードマップ

## 0. このドキュメントの目的とスコープ

### 0.1 目的

現状、`applications/frontend/{reader,admin}` の Next.js サーバーサイドは
`applications/frontend/shared/src/infrastructures/*` の Firebase Admin SDK を通じて
**Firestore に直接アクセス**している。

これを「**ドメインごとに Cloud Run の API（Haskell + gRPC）を立て、Next.js は
API クライアント経由でデータを取得する**」アーキテクチャに段階的に移行する。

### 0.2 期待される効果

| 観点 | 現状 | 移行後 |
|---|---|---|
| データ整合性 | Next.js / バッチ / worker 各々で同じ Firestore に直アクセス | API 経由のみ。ドメイン不変条件を 1 箇所で保証 |
| 認可 | Admin SDK 全権 | gRPC メソッド × IAM 条件で最小権限 |
| デプロイ | reader / admin の Next.js 単位 | ドメイン単位（記事 / メモ / 連載 / ...） |
| 言語選択 | TypeScript のみ | API は Haskell に統一（`search-token-worker` と揃える） |
| 配信境界 | Next.js サーバが信頼境界 | API が境界、Next.js は単なるクライアント |

### 0.3 スコープ外

- **Cloudflare Workers (reader PRD) の継続運用**: 既存のフロント配信は維持。本ロードマップでは
  Workers から Cloud Run API を呼ぶ際の **認証経路** だけを対象とする。
- **既存 Firestore スキーマの変更**: API 抽出と同時に行わない。スキーマ変更は別ロードマップ。
- **マルチリージョン化**: `asia-northeast1` 単一構成を継続。

### 0.4 このドキュメントの読み方

1. **第 1 章 前提・制約** で環境要件と既存資産を確認する
2. **第 2 章 仕様決定事項** で本セッションで合意した設計判断を確認する
3. **第 3 章 全体アーキテクチャ** で論理図を見る
4. **第 4 章 横断的リスク** でセキュリティ・可用性・スケーラビリティ・運用・コスト・設計の
   各観点での懸念と緩和策を読む（実装時に必ず参照すること）
5. **第 5 章 フェーズ別手順** で 0 → 10 の作業を順に進める
6. 各フェーズの完了条件と、巻末の **第 6 章 チェックリスト** で漏れを確認する

詳細なタスク一覧は別ファイル `docs/roadmaps/cloudrun-api-extraction-todo.md` を参照。

---

## 1. 前提・制約

### 1.1 既存資産

| カテゴリ | パス | 役割 |
|---|---|---|
| ドメインモデル | `applications/frontend/shared/src/domains/*` | Zod brand 型、エンティティ、ドメインイベント |
| Firestore リポジトリ | `applications/frontend/shared/src/infrastructures/*` | 抽出対象。記事・メモ・連載・チャプター・タグ・管理者・検索・Analytics・Image・User |
| Workflow | `applications/frontend/shared/src/workflows/*` | 移植対象。`find/findBySlug/search/create/edit/terminate` パターン |
| Server Actions | `applications/frontend/shared/src/actions/*` | Workflow を呼ぶ薄いラッパー。残置（中身を gRPC client 呼び出しに置換） |
| Firestore 初期化 | `applications/frontend/shared/src/providers/infrastructure/firebase-admin.ts` | 最終的に削除対象 |
| Haskell 実装の前例 | `applications/search-token-worker/` | Servant + Firestore、Cloud Run 運用済み。本プロジェクトの参考実装 |
| Terraform モジュール | `infrastructure/modules/cloudrun_service/`, `iam/`, `firestore/`, `secret_manager/`, `artifact_registry/` | 再利用前提 |
| 認証基盤 | Workload Identity Federation（GitHub Actions → GCP は既設）| Cloudflare Workers → GCP の federation を本ロードマップで新設 |
| リリース管理 | `release-please-config.json`（単一パッケージ） | multi-package 化が必要 |

### 1.2 命名規則とコーディング規約

- 略語禁止（`URL` / `UUID` / `ULID` のみ可）
- 識別子型: `XXXIdentifier`（`XXXId` 禁止）
- 自身の識別子フィールド名: `identifier`
- 他モデルへの識別子: `domainName: XXXIdentifier`（`xxxIdentifier` の suffix なし）
- TypeScript で `as any` / `as unknown` 禁止
- Haskell は `applications/search-token-worker/` と同じスタイルに揃える（`ormolu` + `hlint`）

### 1.3 ブランチ戦略

| ブランチ | 用途 |
|---|---|
| `feat/cloudrun-api-extraction`（このブランチ）| ロードマップ / TODO 配置・**フェーズ 0 基盤整備** |
| `feat/api-articles` | フェーズ 1: 記事 API |
| `feat/api-memos` | フェーズ 2: メモ API |
| 以降ドメインごとに `feat/api-<domain>` | フェーズ 3〜9 |
| `feat/api-cleanup` | フェーズ 10: クリーンアップ |

各フェーズは `staging` への PR を起票し、レビュー後にマージ。`main` への昇格は複数フェーズを
まとめて行うか、フェーズ単位で行うかは Release PR の粒度で判断。

---

## 2. 仕様決定事項（合意済み）

本セッション（2026-05-14）でユーザーと合意した設計判断。**実装時に再議論しない**。

### 2.1 API 実装言語: Haskell + gRPC

- フレームワーク候補は `servant-server` + gRPC ライブラリ（実装フェーズ 0 で確定）
- 既存 `search-token-worker` (Haskell/Servant) と同居しやすく、社内に既知の運用例がある
- `applications/api/shared-hs/` に共通ライブラリを配置する cabal multi-package 構成

### 2.2 通信プロトコル: gRPC + Protobuf

- HTTP/2 上の gRPC（Cloud Run はネイティブ対応）
- ブラウザから直接呼ばないため gRPC-Web は当面不要
- TypeScript 側クライアント: `@connectrpc/connect-node` を採用候補とする

### 2.3 スキーマの真実の源: Protobuf

- `proto/<domain>/v1/<entity>.proto` を一次定義
- TS / Haskell の生成コードはあくまで「境界の DTO」
- ドメイン不変条件（ULID 制約、最小長、複合制約）は両言語のドメイン層で重複保持
  - TS 側: `z.brand()` の smart constructor
  - Haskell 側: `newtype` + smart constructor
- 形式的バリデーション（型・長さ・パターン）は `protovalidate (buf validate)` で
  `.proto` に宣言。Haskell 側ランタイムが未成熟なため、Servant 層に手書き
  バリデーターを実装し CI で `.proto` 制約との整合性を検証

### 2.4 API 間認証: Cloud Run IAM (Service-to-Service)

- Caller の SA が `roles/run.invoker` を持つ
- Caller は GCP メタデータサーバから ID Token を取得し、`Authorization: Bearer ...` で送付
- API 側は Cloud Run プラットフォームが自動検証
- `allow-unauthenticated` は**絶対に有効にしない**（Terraform lint で強制）

### 2.5 範囲: 全ドメイン段階的移行（記事 API から）

- フェーズ 1 で記事 API を PoC として完成させる
- 以降、メモ → 連載 → チャプター → タグ → 管理者 / ユーザー → 検索 → Analytics → Image
  の順で 1 ドメインずつ移行
- 移行中は **フィーチャーフラグ**（`USE_API_FOR_<DOMAIN>` 環境変数）で Firestore 直 / API 経由を切替

### 2.6 ローカル開発: Docker Compose 一括起動

- `docker-compose.api.yml`（仮）で Firebase Emulator + 全 Haskell API を起動
- `pnpm dev:api` などのトップレベルコマンドを追加
- API ごとに Compose profile を割り当て、開発者が必要なものだけ起動できるようにする

### 2.7 IDL 配置

```
proto/
  buf.yaml
  buf.gen.yaml
  buf.lock
  articles/v1/article.proto
  articles/v1/article_service.proto
  memos/v1/memo.proto
  ...
  common/v1/timeline.proto
  common/v1/pagination.proto
  common/v1/errors.proto
```

---

## 3. 全体アーキテクチャ

### 3.1 移行前

```text
[Browser]
  -> [Next.js Server Action (Vercel/Cloudflare Workers/Cloud Run)]
       -> [shared/workflows/<domain>]
            -> [shared/infrastructures/<domain>.ts] -- Firebase Admin SDK --> [Firestore]
```

### 3.2 移行後

```text
[Browser]
  -> [Next.js Server Action]
       -> [shared/infrastructures/grpc/<domain>Client]
            -- gRPC + Cloud Run IAM Identity Token -->
       [Cloud Run: api-<domain> (Haskell)]
            -> [internal Workflow + Repository]
                 -- gogol-firestore --> [Firestore]
```

### 3.3 認証経路

| Caller | Auth |
|---|---|
| Cloud Run reader/admin (Next.js) → Cloud Run API | メタデータサーバから ID Token 取得 |
| Cloudflare Workers reader → Cloud Run API | Workload Identity Federation で OIDC を GCP SA にフェデレーション、または BFF（中継 Cloud Run）に統一 |
| GitHub Actions → Terraform / deploy | 既存の Workload Identity Federation |

### 3.4 IAM ロール設計

| SA | 付与ロール | 説明 |
|---|---|---|
| `reader-sa@...` | `roles/run.invoker` を **read 系 API のみ** | reader Next.js が呼ぶ |
| `admin-sa@...` | `roles/run.invoker` を **read/write 両系 API** | admin Next.js が呼ぶ |
| `api-<domain>-sa@...` | `roles/datastore.user` のみ | API が Firestore を読み書き |
| `cf-workers-pool@...` | WIF プロバイダ。reader-sa を impersonate | Cloudflare Workers が使う |

`roles/datastore.owner` は誰にも付与しない。

---

## 4. 横断的リスクと緩和策（実装時に必ず参照）

### 4.1 セキュリティ

| ID | リスク | 緩和策 |
|---|---|---|
| S-1 | これまで Next.js プロセスに閉じていた Firestore アクセスが API 境界として露出 | 全 API で Cloud Run IAM 必須化。`allow-unauthenticated` 禁止。Terraform で `ingress = INTERNAL_AND_CLOUD_LOAD_BALANCING` または `INGRESS_TRAFFIC_ALL` + IAM 強制を選択し、PR レビューで明示確認 |
| S-2 | Cloudflare Workers reader が GCP SA を持たない | Workload Identity Federation で Cloudflare の OIDC を GCP SA にフェデレーション。または admin と同居の中継 Cloud Run（BFF）に統一し、Workers → BFF → API の経路に揃える |
| S-3 | Firestore セキュリティルールが「Admin SDK 全権」前提のまま | `firestore.rules` を全ドキュメント `allow read, write: if false` に変更。API SA は IAM の `roles/datastore.user` 経由で例外的にアクセス（Rules は適用されない）。Next.js / Workers は Firestore に直接触れない |
| S-4 | gRPC のメソッドレベル認可 | 各 RPC メソッドに対し IAM 条件 `request.path.matches('/articles.v1.ArticleService/Find.*')` で限定。または API 側ミドルウェアで caller の SA メールを判定 |
| S-5 | 不正ペイロード侵入 | `protovalidate` で形式バリデーション + ドメイン層 smart constructor で不変条件チェック。CI に `.proto` ルールとサーバ側手書きバリデーターの整合性テスト |
| S-6 | ログに PII / 本文が垂れる | OpenTelemetry の `structured logging` + Cloud Logging の log scrubbing。`Body` フィールドはログ出力しない規約 |
| S-7 | gRPC over public network | Cloud Run 同士は常に HTTPS + IAM。VPC コネクタは当面不要 |
| S-8 | DoS / 過剰使用 | Cloud Run `max_instances` で上限。将来的に Cloud Armor / API Gateway 検討 |
| S-9 | SA 鍵ファイル漏洩 | SA 鍵を作らない。Workload Identity / メタデータサーバのみ使う |

### 4.2 可用性

| ID | リスク | 緩和策 |
|---|---|---|
| A-1 | ネットワーク往復増加によるレイテンシ | gRPC で複数 RPC をまとめるバッチ API を要所に。Next.js `cache()` を維持し ISR を活用 |
| A-2 | Haskell コンテナのコールドスタート | PRD は `min_instances >= 1`。STG は `0` 可 |
| A-3 | Cloud Run / Firestore 障害時の連鎖 | read 系は R2 / Next.js cache に fallback。write 系はリトライ + サーキットブレーカ |
| A-4 | リトライ嵐による Firestore 過負荷 | gRPC client retry は冪等メソッド限定。Create/Edit は冪等キー必須 |
| A-5 | リージョン障害 | `asia-northeast1` 単一は許容。将来課題として記録 |

### 4.3 スケーラビリティ

| ID | リスク | 緩和策 |
|---|---|---|
| SC-1 | N+1 RPC によるファンアウト爆発 | Search/List は cursor pagination。N+1 が見えた箇所はバッチ RPC を proto に追加 |
| SC-2 | Firestore 読み回数増加 | API 側に in-memory LRU を入れるか、Next.js 側 `cache()` で吸収 |
| SC-3 | CI/CD パイプライン本数が ×N | reusable workflow + path-filter でテンプレート化 |

### 4.4 運用

| ID | リスク | 緩和策 |
|---|---|---|
| O-1 | デプロイパイプライン複雑化 | reusable workflow と Terraform module で API 1 つあたりの追加コストを最小化 |
| O-2 | ローカル開発の重さ | Compose profiles で必要な API だけ起動 |
| O-3 | 分散トレース無しでは切り分け不能 | OpenTelemetry → Cloud Trace を Day 1 導入。Trace-ID を gRPC metadata `traceparent` に伝搬 |
| O-4 | release-please が単一パッケージ前提 | API 各サービスを `release-please-config.json` の `packages` に追加 |
| O-5 | Haskell ビルド時間増加 | 共通モジュールは `applications/api/shared-hs/` に。cabal multi-package + Docker buildkit cache |

### 4.5 コスト

| ID | リスク | 緩和策 |
|---|---|---|
| C-1 | `min_instances` × N で常時稼働コスト増 | 本当に必要な API のみ `min_instances=1`。低トラフィック API は `0` |
| C-2 | Artifact Registry のストレージ膨張 | 古い image のライフサイクル設定（30 日保持） |
| C-3 | gRPC egress 課金 | BigQuery export で監視。必要に応じて圧縮（gzip）有効化 |

### 4.6 設計

| ID | 論点 | 結論 |
|---|---|---|
| D-1 | エラー型のマッピング | `AggregateNotFoundError → NOT_FOUND`, `ValidationError → INVALID_ARGUMENT`, `DuplicationError → ALREADY_EXISTS`, `UnexpectedError → INTERNAL`, `OptimisticLockError → ABORTED` |
| D-2 | ブランド型の喪失 | TS 側 Zod brand / Haskell 側 newtype で再ラップ。生コード（`string` のまま）をリポジトリ呼び出しに渡さない |
| D-3 | ページネーション | cursor-based。`ListXxxRequest { string cursor; int32 limit; }` を `common/v1/pagination.proto` に共通化 |
| D-4 | 日時 | `google.protobuf.Timestamp`。TS 側で `Date` / Zod date、Haskell 側で `UTCTime` に変換 |
| D-5 | Versioning | `package <domain>.v1` で開始。`buf breaking` で破壊的変更検知 |
| D-6 | Workflow 層の置き場所 | Haskell 側へ移植。Next.js 側はクライアント wrapper のみ |
| D-7 | 移行戦略 | Repository factory に `useApi: boolean` フラグ。環境変数で段階切替、ロールバック即時 |

---

## 5. フェーズ別手順

### フェーズ 0: 基盤整備（`feat/cloudrun-api-extraction`）

#### 目標

「Hello API」相当の Haskell gRPC サービスが Cloud Run STG にデプロイされ、Next.js から
Cloud Run IAM 認証付きで呼び出せる状態。

#### 主要タスク

1. `proto/` ディレクトリ作成と buf 設定（`buf.yaml`, `buf.gen.yaml`, `buf.lock`）
2. `buf lint` / `buf breaking` を CI に組み込む
3. `applications/api/shared-hs/` cabal package 初期化
   - gRPC server 雛形（`proto3-suite` / `grapesy` / `mu-haskell` から選定）
   - Firestore client ラッパー（`gogol-firestore`）
   - Result monad / Error 型（`Either DomainError`）
   - OpenTelemetry exporter 設定
4. `applications/frontend/shared/src/infrastructures/grpc/` TS クライアントファクトリ
   - `@connectrpc/connect-node` 導入
   - フィーチャーフラグ機構（`USE_API_FOR_<DOMAIN>`）
5. `infrastructure/modules/cloudrun_api_service/`（既存 `cloudrun_service` の派生 or 拡張）
   - `allow-unauthenticated` を許可しない validator
   - IAM 条件式テンプレート
6. `firestore.rules` を「全ドキュメント deny」に切り替えるドラフト（フェーズ 1 で適用）
7. Workload Identity Federation: Cloudflare Workers OIDC → GCP SA
8. Docker Compose 雛形: Firebase Emulator + Haskell API "hello-api"
9. GitHub Actions
   - `api-ci.yml`（Haskell build/test）
   - `api-deploy.yml`（reusable workflow）
   - path-filter で差分検出
10. `release-please-config.json` を multi-package 化
11. セキュリティチェックリスト草案（S-1〜S-9）

#### 完了条件

- `buf lint` が CI で通る
- `applications/api/hello-api/` が Cloud Run STG にデプロイされる
- Next.js から `hello-api` への gRPC 呼び出しが Cloud Run IAM 認証で成功する
- `firestore.rules` の新形式が `staging` Firestore で動作確認できる（API SA のみ通る）
- `allow-unauthenticated` を許す PR は CI で reject される

---

### フェーズ 1: 記事 API（PoC）（`feat/api-articles`）

#### 目標

記事ドメインが完全に API 経由になり、CI / E2E / パフォーマンステスト全てパス。

#### 主要タスク

1. **Protobuf 定義**
   - `proto/articles/v1/article.proto`
   - `proto/articles/v1/article_service.proto`
   - RPC: `Find`, `FindBySlug`, `Search`, `Create`, `Edit`, `Terminate`
   - `protovalidate` ルール（ULID、タイトル長、本文長、status enum）
2. **Haskell 実装** (`applications/api/articles/`)
   - Domain model（`newtype ArticleIdentifier`, etc）
   - Workflow 移植
   - Repository（Firestore client）
   - gRPC server endpoint
   - ユニットテスト 80%+ カバレッジ
   - 統合テスト（Firestore Emulator）
3. **Next.js 切替**
   - `applications/frontend/shared/src/infrastructures/articles.ts` に gRPC client 実装
   - フィーチャーフラグ `USE_API_FOR_ARTICLES` で Firestore 直 / API 経由を切替
   - ダブルリード期間にシャドー比較ログを Cloud Logging に出力
4. **インフラ**
   - Terraform で `stg-api-articles`, `prd-api-articles`
   - IAM 権限: `reader-sa` は read 系のみ、`admin-sa` は read/write 両方
   - Secret Manager（必要なら）
5. **デプロイ・切替**
   - STG 全量切替（最低 1 週間ダブルリード）
   - PRD カナリー 10% → 50% → 100%
   - 旧 Firestore 直アクセスコード削除
   - パフォーマンス測定（p50/p99 latency before/after）
6. **完了処理**
   - CI / E2E / perf 全パス
   - CHANGELOG / ADR 起票（`docs/adr/` または `docs/internal/done/`）

#### 完了条件

- 記事系の Next.js 経路が **API 経由 100%** で動作
- `shared/infrastructures/articles.ts` から Firebase Admin SDK 依存が消える
- パフォーマンスが p99 で 1.5x 以内に収まる（目標値）
- セキュリティチェックリスト S-1〜S-9 が全項目満たされる

---

### フェーズ 2: メモ API（`feat/api-memos`）

フェーズ 1 と同じテンプレ。`proto/memos/v1/`、`applications/api/memos/`。

### フェーズ 3: 連載 API（`feat/api-series`）

`proto/series/v1/`、`applications/api/series/`。

### フェーズ 4: チャプター API（`feat/api-chapters`）

`proto/chapters/v1/`、`applications/api/chapters/`。

### フェーズ 5: タグ・属性 API（`feat/api-tags`）

`proto/tags/v1/`、`applications/api/tags/`。Category/Attribute も含む。

### フェーズ 6: 管理者・ユーザー API（`feat/api-users`）

`proto/admin/v1/`、`proto/users/v1/`、`applications/api/users/`。
**認可境界の正念場**: admin RPC は admin-sa のみ呼べる IAM 設定を厳密に。

### フェーズ 7: 検索 API（`feat/api-search`）

`proto/search/v1/`。既存 `applications/search-token-worker/` と統合または並走。
search-index / search-token を扱う。

### フェーズ 8: Analytics API（`feat/api-analytics`）

`proto/analytics/v1/`、`applications/api/analytics/`。PV/UV/エンゲージメント。

### フェーズ 9: Image / Document API（`feat/api-image`）

`proto/image/v1/`、`proto/document/v1/`、`applications/api/image/`。
画像メタデータと URL 管理。

---

### フェーズ 10: クリーンアップ（`feat/api-cleanup`）

#### 主要タスク

- `applications/frontend/shared/` から Firebase Admin SDK 依存を削除
- `applications/frontend/shared/src/providers/infrastructure/firebase-admin.ts` 削除
- `applications/frontend/shared/src/infrastructures/*` の Firestore 直リポジトリ削除
- ローカル開発から Firestore Emulator 直結を廃止（API 経由のみ）
- `firestore.rules` を最終形に固定
- `release-please` の admin/reader パッケージ依存を更新
- ADR を `docs/internal/done/` に移動

#### 完了条件

- `pnpm depcheck` / `pnpm knip` で Firebase Admin SDK 関連の未使用エクスポートが 0
- Next.js 側で「Firestore に直接触れる」grep ヒット 0
- 全 E2E パス

---

## 6. チェックリスト

詳細は `docs/roadmaps/cloudrun-api-extraction-todo.md` を参照。
ここでは各フェーズの **ゲート** だけを記載。

### フェーズ 0 ゲート

- [ ] `buf lint` / `buf breaking` が CI で動く
- [ ] `applications/api/hello-api/` が Cloud Run STG にデプロイされる
- [ ] Next.js → hello-api が IAM 認証で疎通する
- [ ] Workload Identity Federation（Cloudflare Workers → GCP）が動く
- [ ] `allow-unauthenticated` を有効化する Terraform 変更が CI で reject される

### フェーズ 1 ゲート（記事 API）

- [ ] `.proto` 定義（Find/FindBySlug/Search/Create/Edit/Terminate）
- [ ] Haskell 実装 + ユニットテスト 80%+
- [ ] Next.js shared/infrastructures/articles.ts が gRPC client に置換
- [ ] STG ダブルリードでシャドー比較に差分なし
- [ ] PRD カナリー 100% 切替
- [ ] パフォーマンス p99 が 1.5x 以内
- [ ] セキュリティチェックリスト S-1〜S-9 完了

### フェーズ 2〜9 ゲート

各ドメインでフェーズ 1 と同じチェックを完遂。

### フェーズ 10 ゲート

- [ ] Firebase Admin SDK が `applications/frontend/` の package.json から削除
- [ ] `firestore.rules` 最終形が PRD に適用
- [ ] E2E 全パス
- [ ] ADR が `docs/internal/done/` に移動

---

## 7. 関連ドキュメント

- 参考実装: `applications/search-token-worker/`（Haskell + Servant + Cloud Run の前例）
- 参考ロードマップ: `docs/roadmaps/search-token-eventarc-haskell-servant.md`
- Terraform 既存モジュール: `infrastructure/modules/cloudrun_service/`, `iam/`, `firestore/`
- TODO チェックリスト: `docs/roadmaps/cloudrun-api-extraction-todo.md`
