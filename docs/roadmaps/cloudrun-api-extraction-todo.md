# Cloud Run API 抽出 TODO チェックリスト

> 詳細な背景は `docs/roadmaps/cloudrun-api-extraction.md` を参照。
> このファイルは **進捗トラッキング用** のチェックリスト。

## フェーズ 0: 基盤整備（`feat/cloudrun-api-extraction`）

### Protobuf 規律

- [ ] `proto/` ディレクトリを作成
- [ ] `proto/buf.yaml` を配置（lint / breaking ルール）
- [ ] `proto/buf.gen.yaml` を配置（TS / Haskell 生成プラグイン）
- [ ] `proto/buf.lock` を初期化
- [ ] `proto/common/v1/timeline.proto` を作成（共通の created_at/updated_at/version）
- [ ] `proto/common/v1/pagination.proto` を作成（cursor-based 共通）
- [ ] `proto/common/v1/errors.proto` を作成（error details 共通）
- [ ] `buf lint` を CI に追加
- [ ] `buf breaking` を main ブランチ比較で CI に追加
- [ ] protovalidate ルール記述ガイドラインを `proto/README.md` に書く

### Haskell 共通基盤

- [ ] `applications/api/` ディレクトリを新設
- [ ] `applications/api/shared-hs/` cabal package を初期化
- [ ] gRPC server ライブラリを選定（`proto3-suite` / `grapesy` / `mu-haskell`）
- [ ] gRPC server 雛形を `shared-hs/src/Grpc/Server.hs` に
- [ ] Firestore client ラッパー（`gogol-firestore`）を `shared-hs/src/Infra/Firestore.hs` に
- [ ] Result monad / Error 型を `shared-hs/src/Result.hs` に
  - [ ] `DomainError` sum type
  - [ ] `mapDomainErrorToGrpcStatus` 関数（D-1 のマッピング）
- [ ] OpenTelemetry exporter 設定（`shared-hs/src/Telemetry.hs`）
- [ ] gRPC metadata `traceparent` 伝搬ミドルウェア
- [ ] PII マスキング規約（`shared-hs/src/Logger.hs`）
- [ ] 統合テスト用 Firestore Emulator フィクスチャ（`shared-hs/test/`）

### Haskell 認証基盤（`shared-hs/Auth/`）

- [ ] `shared-hs/src/Auth/Firebase.hs` を実装
  - [ ] Firebase session cookie の JWT 検証（RS256）
  - [ ] JWKs 公開鍵を `https://www.googleapis.com/identitytoolkit/v3/relyingparty/publicKeys` から取得して TTL キャッシュ
  - [ ] `iss = https://session.firebase.google.com/<PROJECT_ID>` 検証
  - [ ] `aud = <PROJECT_ID>` 検証
  - [ ] `exp` 検証 / `iat` 妥当性
  - [ ] admin custom claim 確認関数
- [ ] `shared-hs/src/Auth/Context.hs` を実装
  - [ ] gRPC metadata から `x-firebase-session-cookie` を取り出し検証
  - [ ] `AuthContext { uid, email, role, callerSA }` を構築
  - [ ] 匿名 read 経路用の `AnonymousAuthContext`
- [ ] `shared-hs/src/Auth/Guard.hs` を実装
  - [ ] `requireAdmin :: AuthContext -> Either DomainError ()`
  - [ ] `requireRole :: Role -> AuthContext -> Either DomainError ()`
  - [ ] `requireOwner :: ResourceOwner -> AuthContext -> Either DomainError ()`
- [ ] 実 `admin_session` cookie を使った統合テスト（Firebase Auth Emulator）

### TypeScript gRPC client 基盤

- [ ] `applications/frontend/shared/` に `@connectrpc/connect-node` を追加
- [ ] `shared/src/infrastructures/grpc/` ディレクトリを新設
- [ ] gRPC client ファクトリ
  - [ ] Cloud Run IAM Identity Token 取得（metadata server / WIF）
  - [ ] `x-firebase-session-cookie` metadata を Server Action のリクエストスコープから注入する仕組み
  - [ ] `x-trace-id` を OpenTelemetry traceparent から注入
- [ ] フィーチャーフラグ機構（`USE_API_FOR_<DOMAIN>` 環境変数）
- [ ] Repository factory に `useApi: boolean` スイッチを追加
- [ ] シャドー比較ロガー（Firestore 直 vs API の差分を Cloud Logging に）

### インフラ（Terraform）

- [ ] `infrastructure/modules/cloudrun_api_service/` モジュール新設（既存 `cloudrun_service` 派生）
  - [ ] `allow-unauthenticated` を有効化できない validator
  - [ ] IAM 条件式テンプレート（メソッドレベル制限）
  - [ ] OpenTelemetry / Cloud Trace 権限テンプレート
- [ ] `infrastructure/modules/iam/` を更新
  - [ ] `reader-sa` の read 系 API への `roles/run.invoker` テンプレート
  - [ ] `admin-sa` の read/write 系 API への `roles/run.invoker` テンプレート
  - [ ] `api-<domain>-sa` の `roles/datastore.user` テンプレート
- [ ] Workload Identity Federation（Cloudflare Workers OIDC → GCP SA）の Terraform リソース
- [ ] `firestore.rules` の新形式ドラフト（全 deny + 注釈）を作成
- [ ] Cloud Logging / Cloud Trace の権限設定
- [ ] Artifact Registry のライフサイクル設定（30 日保持）

### CI/CD

- [ ] `.github/workflows/api-ci.yml` を新設（Haskell build/test、Docker buildkit cache）
- [ ] `.github/workflows/api-deploy.yml` を reusable workflow として新設
- [ ] path-filter で「変更があった API だけビルド・デプロイ」
- [ ] `release-please-config.json` を multi-package 化
  - [ ] `applications/api/<domain>/` を packages に追加（テンプレート）
- [ ] Terraform CI で `allow-unauthenticated` 有効化 PR を reject する lint

### ローカル開発

- [ ] `docker-compose.api.yml` 新設（Firebase Emulator + API skeleton）
- [ ] Compose profile（`articles`, `memos`, ...）で必要 API だけ起動
- [ ] `pnpm dev:api` コマンド追加（package.json scripts）
- [ ] ローカル用 mock IAM Identity Token 戦略を README にメモ

### Hello API（PoC）

- [ ] `applications/api/hello-api/` を新設
- [ ] `proto/hello/v1/hello.proto`（`SayHello(name) -> message`）
- [ ] Haskell 実装
- [ ] Cloud Run STG にデプロイ
- [ ] Next.js から IAM 認証で呼び出して 200 OK を確認

### セキュリティチェックリスト草案

- [ ] S-1: `allow-unauthenticated` 禁止の Terraform lint が CI で動く
- [ ] S-2: Cloudflare Workers → Cloud Run の認証経路が動作
- [ ] S-3: `firestore.rules` 新形式が staging に適用済み
- [ ] S-4: gRPC メソッドレベル IAM 条件のテンプレートが用意できている
- [ ] S-5: `protovalidate` 規約と Servant 側手書きバリデーターの整合性テスト雛形
- [ ] S-6: PII ログマスキング規約を `shared-hs/src/Logger.hs` に
- [ ] S-7: Cloud Run の `ingress` 設定方針を README に
- [ ] S-8: `max_instances` テンプレート値を Terraform に
- [ ] S-9: SA 鍵ファイルを作成・コミットしない CI lint

### フェーズ 0 完了条件

- [ ] `buf lint` / `buf breaking` が CI で動く
- [ ] `applications/api/hello-api/` が Cloud Run STG にデプロイされる
- [ ] Next.js → hello-api が IAM 認証で疎通する
- [ ] Workload Identity Federation（Cloudflare Workers → GCP）が動く
- [ ] セキュリティチェックリスト草案がすべて埋まる

---

## フェーズ 1: 記事 API（`feat/api-articles`）

### Protobuf

- [ ] `proto/articles/v1/article.proto`（message 定義）
  - [ ] `Article` message（identifier, slug, title, body, timeline, version, status, tags, series, chapter）
  - [ ] `ArticleStatus` enum
  - [ ] `ArticleSnapshot`（永続化形）
- [ ] `proto/articles/v1/article_service.proto`（service 定義）
  - [ ] `rpc Find(FindRequest) returns (Article)`
  - [ ] `rpc FindBySlug(FindBySlugRequest) returns (Article)`
  - [ ] `rpc Search(SearchRequest) returns (SearchResponse)` (cursor pagination)
  - [ ] `rpc Create(CreateRequest) returns (Article)`
  - [ ] `rpc Edit(EditRequest) returns (Article)`
  - [ ] `rpc Terminate(TerminateRequest) returns (google.protobuf.Empty)`
- [ ] protovalidate ルール
  - [ ] identifier: ULID pattern
  - [ ] slug: 長さ・パターン
  - [ ] title: 最小・最大長
  - [ ] body: 最小長（写し漏れ防止）
  - [ ] status enum defined_only
- [ ] `buf lint` 通過
- [ ] `buf breaking` 通過

### Haskell 実装（`applications/api/articles/`）

- [ ] cabal package 初期化
- [ ] Domain model
  - [ ] `newtype ArticleIdentifier = ArticleIdentifier Text` + smart constructor
  - [ ] `newtype ArticleTitle = ArticleTitle Text` + smart constructor
  - [ ] `newtype ArticleSlug = ArticleSlug Text` + smart constructor
  - [ ] `data Article` 集約ルート
- [ ] Workflow 層移植
  - [ ] FindWorkflow
  - [ ] FindBySlugWorkflow
  - [ ] SearchWorkflow
  - [ ] CreateWorkflow
  - [ ] EditWorkflow
  - [ ] TerminateWorkflow
- [ ] Repository（Firestore client）
  - [ ] find/findBySlug/search/persist/terminate
  - [ ] バージョン管理（オプティミスティックロック）
- [ ] gRPC server endpoint 実装
  - [ ] Protobuf ↔ Domain model マッパー
  - [ ] DomainError → gRPC Status マッピング
- [ ] ユニットテスト 80%+ カバレッジ
- [ ] 統合テスト（Firestore Emulator）
- [ ] Servant 側手書きバリデーター + `.proto` 整合性テスト
- [ ] `Dockerfile`（マルチステージ）
- [ ] OpenTelemetry exporter 設定

### TypeScript 切替

- [ ] `applications/frontend/shared/src/infrastructures/grpc/articles.ts` 新規
  - [ ] `createArticleApiClient(channel)` ファクトリ
  - [ ] Protobuf 型 ↔ ドメイン型マッパー
- [ ] `applications/frontend/shared/src/infrastructures/articles.ts` を切替
  - [ ] `useApi` フラグで Firestore 直 / API 経由を分岐
  - [ ] ダブルリード期間のシャドー比較ロガー
- [ ] フィーチャーフラグ `USE_API_FOR_ARTICLES`
  - [ ] STG: 環境変数で ON
  - [ ] PRD: 当初 OFF、カナリー時に ON

### インフラ

- [ ] Terraform で `stg-api-articles` Cloud Run リソース
- [ ] Terraform で `prd-api-articles` Cloud Run リソース
- [ ] IAM 権限
  - [ ] `reader-sa` が read 系 RPC（Find/FindBySlug/Search）を呼べる
  - [ ] `admin-sa` が全 RPC を呼べる
  - [ ] それ以外は拒否
- [ ] `api-articles-sa` に `roles/datastore.user` を付与
- [ ] Secret Manager（必要なら）
- [ ] Cloud Trace / Cloud Logging 権限

### CI/CD

- [ ] path-filter で `applications/api/articles/` 変更時のみビルド
- [ ] STG デプロイ自動化
- [ ] PRD デプロイは手動承認

### デプロイ・切替

- [ ] STG 全量切替
- [ ] STG ダブルリード（最低 1 週間）でシャドー差分 0 確認
- [ ] PRD カナリー 10%
- [ ] PRD カナリー 50%
- [ ] PRD カナリー 100%
- [ ] 旧 Firestore 直アクセスコード削除
- [ ] パフォーマンス測定（p50/p99 latency before/after）が 1.5x 以内

### 完了

- [ ] CI 全パス
- [ ] E2E 全パス
- [ ] セキュリティチェックリスト S-1〜S-9 完了
- [ ] CHANGELOG / ADR 起票

---

## フェーズ 2: メモ API（`feat/api-memos`）

フェーズ 1 と同じ粒度で、メモドメインに対して以下を完了:

- [ ] `proto/memos/v1/` 定義（Memo entity + Service）
- [ ] `applications/api/memos/` Haskell 実装
- [ ] `shared/infrastructures/memos.ts` を gRPC client に切替
- [ ] Terraform `stg-api-memos`, `prd-api-memos`
- [ ] IAM 権限
- [ ] STG ダブルリード → PRD カナリー → 100% 切替
- [ ] 旧コード削除
- [ ] CI / E2E / セキュリティチェックリスト完了

---

## フェーズ 3: 連載 API（`feat/api-series`）

- [ ] `proto/series/v1/` 定義
- [ ] `applications/api/series/` Haskell 実装
- [ ] `shared/infrastructures/series.ts` を gRPC client に切替
- [ ] Terraform リソース
- [ ] IAM 権限
- [ ] STG → PRD 切替
- [ ] 旧コード削除
- [ ] CI / E2E / セキュリティチェックリスト完了

---

## フェーズ 4: チャプター API（`feat/api-chapters`）

- [ ] `proto/chapters/v1/` 定義
- [ ] `applications/api/chapters/` Haskell 実装
- [ ] `shared/infrastructures/chapter.ts` を gRPC client に切替
- [ ] Terraform リソース
- [ ] IAM 権限
- [ ] STG → PRD 切替
- [ ] 旧コード削除
- [ ] CI / E2E / セキュリティチェックリスト完了

---

## フェーズ 5: タグ・属性 API（`feat/api-tags`）

- [ ] `proto/tags/v1/`, `proto/attributes/v1/` 定義
- [ ] `applications/api/tags/` Haskell 実装
- [ ] `shared/infrastructures/tags.ts` を gRPC client に切替
- [ ] Terraform リソース
- [ ] IAM 権限
- [ ] STG → PRD 切替
- [ ] 旧コード削除
- [ ] CI / E2E / セキュリティチェックリスト完了

---

## フェーズ 6: 管理者・ユーザー・auth API（`feat/api-users`）

**認可境界の最重要フェーズ**。admin 限定 RPC を厳密に。`auth-api` でセッション管理を引き受ける。

### users / admin API

- [ ] `proto/admin/v1/`, `proto/users/v1/` 定義
- [ ] `applications/api/users/` Haskell 実装
- [ ] `shared/infrastructures/admin.ts` を gRPC client に切替
- [ ] Terraform リソース
- [ ] IAM 権限
  - [ ] admin RPC は `admin-sa` 限定（reader-sa は呼べない）
  - [ ] 認可テスト（reader-sa から admin RPC を呼んで `PERMISSION_DENIED`）
- [ ] STG → PRD 切替
- [ ] 旧コード削除
- [ ] CI / E2E / セキュリティチェックリスト完了

### auth API（`applications/api/auth/`）

- [ ] `proto/auth/v1/auth_service.proto` 定義
  - [ ] `rpc IssueSession(IssueSessionRequest) returns (IssueSessionResponse)` (`firebaseIdToken` → `sessionCookie`, `expiresAt`)
  - [ ] `rpc VerifySession(VerifySessionRequest) returns (User)` (`sessionCookie` → `User { uid, email, role }`)
  - [ ] `rpc RevokeSession(RevokeSessionRequest) returns (google.protobuf.Empty)`
- [ ] Haskell 実装
  - [ ] Identity Platform REST (`createSessionCookie`) を直叩き
  - [ ] `shared-hs/Auth/Firebase.hs` を共用して session cookie 検証
  - [ ] メールホワイトリスト判定（`OIDC_ALLOWED_EMAILS` 互換）
  - [ ] `revokeRefreshTokens` REST 呼び出し
- [ ] Next.js Server Action 移行
  - [ ] `applications/frontend/admin/src/actions/auth.ts` の `login()` を `authApiClient.IssueSession` 呼び出しに置換
  - [ ] cookie の `set` のみ Next.js に残す（cookie 文字列は auth-api 戻り値を使用）
  - [ ] `getSession()` を `authApiClient.VerifySession` に置換
  - [ ] `logout()` を `authApiClient.RevokeSession` + cookie delete に置換
- [ ] Terraform リソース（`stg-api-auth`, `prd-api-auth`）
- [ ] IAM 権限
  - [ ] reader-sa からは `VerifySession` のみ
  - [ ] admin-sa は全 RPC
  - [ ] auth-api SA は Identity Platform 操作権限
- [ ] E2E 認可テスト
  - [ ] reader-sa → admin RPC で `PERMISSION_DENIED`
  - [ ] 期限切れ cookie で write RPC で `UNAUTHENTICATED`
  - [ ] 別ユーザーの記事編集で `PERMISSION_DENIED`（所有権境界）
- [ ] CI / E2E / セキュリティチェックリスト完了

---

## フェーズ 7: 検索 API（`feat/api-search`）

- [ ] `proto/search/v1/` 定義（search-index + search-token）
- [ ] `applications/api/search/` Haskell 実装
- [ ] 既存 `applications/search-token-worker/` との関係整理（統合 or 並走）
- [ ] `shared/infrastructures/search-*.ts` を gRPC client に切替
- [ ] Terraform リソース
- [ ] IAM 権限
- [ ] STG → PRD 切替
- [ ] 旧コード削除
- [ ] CI / E2E / セキュリティチェックリスト完了

---

## フェーズ 8: Analytics API（`feat/api-analytics`）

- [ ] `proto/analytics/v1/` 定義（PV/UV/エンゲージメント）
- [ ] `applications/api/analytics/` Haskell 実装
- [ ] `shared/infrastructures/analytics-*.ts` を gRPC client に切替
- [ ] Terraform リソース
- [ ] IAM 権限
- [ ] STG → PRD 切替
- [ ] 旧コード削除
- [ ] CI / E2E / セキュリティチェックリスト完了

---

## フェーズ 9: Image / Document API（`feat/api-image`）

- [ ] `proto/image/v1/`, `proto/document/v1/` 定義
- [ ] `applications/api/image/` Haskell 実装
- [ ] `shared/infrastructures/image.ts`, `document.ts` を gRPC client に切替
- [ ] Terraform リソース
- [ ] IAM 権限
- [ ] STG → PRD 切替
- [ ] 旧コード削除
- [ ] CI / E2E / セキュリティチェックリスト完了

---

## フェーズ 10: クリーンアップ（`feat/api-cleanup`）

- [ ] `applications/frontend/shared/package.json` から `firebase-admin` を削除
- [ ] `applications/frontend/admin/package.json` から `firebase-admin` を削除
- [ ] `applications/frontend/shared/src/providers/infrastructure/firebase-admin.ts` を削除
- [ ] `applications/frontend/admin/src/acl/oidc/server.ts` の Firebase Admin SDK 直接呼び出しを削除（`auth-api` 経由に統一済みであることを再確認）
- [ ] `applications/frontend/shared/src/aspects/auth/session.ts`（レガシー Firestore 実装）を削除
- [ ] `applications/frontend/shared/src/infrastructures/*` から Firestore 直実装を削除
- [ ] ローカル開発で Firestore Emulator 直結を廃止（API 経由のみ）
- [ ] `firestore.rules` を最終形（全 deny）に固定
- [ ] `pnpm depcheck` / `pnpm knip` で Firebase 関連の未使用依存 0 を確認
- [ ] Next.js コードベース全体に対し `grep -r 'firebase-admin'` ヒット 0 を確認
- [ ] ADR を `docs/internal/done/` に移動
- [ ] CHANGELOG に「Firestore 直アクセス完全廃止」を記録
- [ ] E2E 全パス
- [ ] パフォーマンス測定（移行前との比較）

### フェーズ 10 完了条件

- [ ] Firebase Admin SDK 依存が `applications/frontend/` から完全に消える
- [ ] `firestore.rules` 最終形が PRD に適用済み
- [ ] E2E 全パス
- [ ] このロードマップを `docs/internal/done/` に移動可能な状態
