# インフラ構成設計書

作成日: 2026-02-20
最終更新日: 2026-02-21

------------------------------------------------------------------------

## 1. 概要

本ドキュメントは、本アプリケーションのインフラ構成をまとめたものです。

設計方針：

-   PRDの reader（一般ユーザー向け）は **Cloudflare Workers (OpenNext)** でホスティング
-   PRDの admin（管理者向け）は **Cloud Run** でホスティング
-   STGは **Cloud Run** で統合ホスティング
-   イベント処理ワーカーは **Cloud Run**（STG/PRD共通）
-   イベント駆動アーキテクチャ（Pub/Sub + Eventarc）
-   秘密情報は **Secret Manager** で管理
-   IaCは **Terraform** で管理
-   独自ドメインは後回し（初期は無料URLで運用）

環境：

-   STG（ステージング）— Cloud Run (`*.run.app`)
-   PRD（本番）— reader: Cloudflare Workers (`*.workers.dev`) / admin: Cloud Run (`*.run.app`)

### 1.1 reader のみ Workers を採用する理由

-   reader は Firebase client SDK のみを使用しており、firebase-admin に依存しない
-   firebase-admin SDK は Cloudflare Workers 上で動作しない（`node:http2` / gRPC 未実装）
-   admin は firebase-admin / @google-cloud/pubsub に依存するため Cloud Run が必須
-   reader を Workers に配置することでエッジ配信による低レイテンシを実現

------------------------------------------------------------------------

## 2. 使用クラウド

### 2.1 Cloudflare

用途：

-   Cloudflare Workers（PRD reader ホスティング）
-   DNS 管理（独自ドメイン取得後）
-   TLS（HTTPS証明書管理）
-   WAF / Rate Limit（PRDのみ、独自ドメイン取得後に有効化）
-   静的アセットのキャッシュ

### 2.2 Google Cloud Platform（Firebase含む）

用途：

-   Cloud Run（STG全サービス / PRD admin / イベント処理ワーカー）
-   Pub/Sub
-   Eventarc
-   Firestore
-   Firebase Authentication
-   Firebase Storage（画像等のオブジェクトストレージ）
-   Secret Manager
-   Artifact Registry
-   Cloud Build
-   IAM
-   Cloud Logging

------------------------------------------------------------------------

## 3. 全体アーキテクチャ

### 3.1 PRD

```
[reader — 一般ユーザー向け]
Client → Cloudflare Workers (Next.js via OpenNext)
           → Firestore / Firebase Auth / Firebase Storage (client SDK)

[admin — 管理者向け]
Client → Cloud Run (Next.js SSR + Server Actions)
           → Firestore / Firebase Auth / Firebase Storage (firebase-admin)
           → Pub/Sub → Eventarc → Cloud Run (イベント処理ワーカー)
```

### 3.2 STG

```
Client → Cloud Run (reader: Next.js SSR)
           → Firestore / Firebase Auth / Firebase Storage (client SDK)

Client → Cloud Run (admin: Next.js SSR + Server Actions)
           → Firestore / Firebase Auth / Firebase Storage (firebase-admin)
           → Pub/Sub → Eventarc → Cloud Run (イベント処理ワーカー)
```

------------------------------------------------------------------------

## 4. ホスティング・ドメイン

### 4.1 ドメイン構成（無料URL）

| サービス | 環境 | ホスティング | サービス名 | URL |
|---|---|---|---|---|
| reader | STG | Cloud Run | `stg-lihs-hut` | `stg-lihs-hut-<hash>.a.run.app` |
| reader | PRD | Cloudflare Workers | `lihs-hut` | `lihs-hut.<account>.workers.dev` |
| admin | STG | Cloud Run | `stg-lihs-hut-landlord` | `stg-lihs-hut-landlord-<hash>.a.run.app` |
| admin | PRD | Cloud Run | `lihs-hut-landlord` | `lihs-hut-landlord-<hash>.a.run.app` |

### 4.2 将来の独自ドメイン構成

必要になったタイミングでCloudflare Registrarから取得する。

レジストラ：Cloudflare Registrar（原価提供）

TLD候補：`.com`（~$10/年/ドメイン）、`.app`、`.dev` 等（未確定）

------------------------------------------------------------------------

## 5. デプロイ対象サービス

### 5.1 reader（一般ユーザー用）

| 項目 | STG | PRD |
|---|---|---|
| デプロイ先 | Cloud Run | Cloudflare Workers |
| ランタイム | Docker コンテナ (standalone) | OpenNext |
| Firebase SDK | client SDK | client SDK |
| URL | `*.run.app` | `*.workers.dev` |

firebase-admin を使用しないため、Workers 上で問題なく動作する。

### 5.2 admin（管理者用）

| 項目 | STG | PRD |
|---|---|---|
| デプロイ先 | Cloud Run | Cloud Run |
| ランタイム | Docker コンテナ (standalone) | Docker コンテナ (standalone) |
| Firebase SDK | firebase-admin + client SDK | firebase-admin + client SDK |
| URL | `*.run.app` | `*.run.app` |

firebase-admin / @google-cloud/pubsub に依存するため、STG/PRDともにCloud Runで稼働。

### 5.3 イベント処理ワーカー

イベント処理ワーカーはSTG/PRDともにCloud Runで稼働する。
イベント種別ごとに個別のCloud Runサービスとして分離する。

| サービス名 | 役割 | 購読トピック |
|---|---|---|
| `image-cleanup-worker` | 未使用画像の削除 | `image-events` |
| `worker` | 汎用イベント処理（Firestore更新、外部API連携等） | `app-events` |

共通仕様：

-   デプロイ先：Cloud Run（STG/PRD共通）
-   Eventarc経由でイベント受信
-   非公開サービス（インターネットからアクセス不可）
-   必要に応じてワーカーを追加可能

------------------------------------------------------------------------

## 6. イベント駆動構成

### 6.1 Pub/Sub

トピックはドメイン単位で分離し、1トピックに対して複数のサブスクリプションを設定可能とする。

トピック一覧：

| トピック名 | 用途 |
|---|---|
| `image-events` | 画像関連イベント（アップロード、削除等） |
| `article-events` | 記事関連イベント |
| `app-events` | 汎用ドメインイベント |

構成例：

```
image-events トピック
  ├── image-cleanup-worker-sub → image-cleanup-worker
  └── (将来) image-thumbnail-worker-sub → image-thumbnail-worker

article-events トピック
  └── article-worker-sub → article-worker
```

### 6.2 Eventarc

各ワーカーに対応するEventarcトリガーを作成する。

イベントタイプ：

google.cloud.pubsub.topic.v1.messagePublished

イベントフロー：

Pub/Sub (ドメイントピック) → Eventarc → Cloud Run (対応するイベント処理ワーカー)

### 6.3 リトライ戦略

Pub/Sub標準のリトライポリシーを使用する。デッドレターキュー（DLQ）は初期構成では導入せず、必要に応じて追加する。

------------------------------------------------------------------------

## 7. データ層

### 7.1 Firestore

-   メインデータベース
-   環境ごとに分離
-   インデックス管理
-   セキュリティルール管理
-   PRDのみ自動バックアップ（スケジュールドエクスポート → GCS）

### 7.2 Firebase Authentication

-   ユーザー認証
-   Email/Password / OAuth
-   カスタムクレームで権限分離

### 7.3 Firebase Storage

-   画像等のオブジェクトストレージ（実体はGoogle Cloud Storage）
-   クライアントからFirebase SDKで直接アップロード
-   Security Rulesによるアクセス制御：
    -   アップロード：認証済みユーザーのみ
    -   読み取り：公開（公開画像）/ 認証済みユーザーのみ（プライベート画像）
-   環境ごとにバケット分離
-   初期はFirebase Storage URLで直接配信（CDNは独自ドメイン取得後に検討）

------------------------------------------------------------------------

## 8. Secret Manager

管理対象例：

-   外部APIキー
-   JWT署名鍵
-   Cookie暗号化キー
-   Webhook検証シークレット

Cloud Runには環境変数として注入。
Cloudflare Workers（reader PRD）には Wrangler の secrets 機能で注入。

必要IAM権限（Cloud Run用）：

roles/secretmanager.secretAccessor

------------------------------------------------------------------------

## 9. STG / PRD 分離

### 9.1 GCPプロジェクト分離

-   hut-stg
-   hut-prd

### 9.2 環境ごとの差分

| 項目 | STG | PRD |
|---|---|---|
| reader ホスティング | Cloud Run | Cloudflare Workers |
| admin ホスティング | Cloud Run | Cloud Run |
| reader URL | `*.run.app` | `*.workers.dev` |
| admin URL | `*.run.app` | `*.run.app` |
| イベント処理ワーカー | Cloud Run | Cloud Run |
| WAF / Rate Limit | なし | 有効（独自ドメイン取得後） |
| Firestoreバックアップ | なし | 日次自動エクスポート |
| シークレット管理 | Secret Manager | Wrangler secrets（reader） + Secret Manager（admin/ワーカー） |

------------------------------------------------------------------------

## 10. CI/CDフロー

### 10.1 ツール構成

| ツール | 役割 |
|---|---|
| GitHub Actions | テスト、lint、型チェック（PR時自動実行） |
| Cloud Build | Dockerイメージビルド → Artifact Registry → Cloud Runデプロイ（admin / イベント処理ワーカー / STG reader） |
| Wrangler (Cloudflare CLI) | PRD reader: Next.jsビルド → OpenNext変換 → Workersデプロイ |

### 10.2 ブランチ戦略

| ブランチ | デプロイ先 | トリガー |
|---|---|---|
| `staging` | STG環境（全サービス Cloud Run） | 手動発火 |
| `main` | PRD環境（reader: Workers / admin: Cloud Run） | 手動発火 |

### 10.3 STGデプロイフロー

1.  GitHub Actionsでテスト / lint 実行（PR時自動）
2.  手動トリガーでCloud Buildパイプライン発火
3.  Dockerイメージ作成（reader, admin, イベント処理ワーカー）
4.  Artifact RegistryへPush
5.  Cloud Runへデプロイ
6.  Terraform apply
7.  Firestoreルール / インデックス反映

### 10.4 PRDデプロイフロー

1.  GitHub Actionsでテスト / lint 実行（PR時自動）
2.  手動トリガーでデプロイパイプライン発火
3.  reader: Next.jsビルド + OpenNext変換 → Wrangler でWorkersへデプロイ
4.  admin / イベント処理ワーカー: Cloud Buildで Cloud Runへデプロイ
5.  Terraform apply
6.  Firestoreルール / インデックス反映

------------------------------------------------------------------------

## 11. 監視・ログ

初期構成はCloud Loggingの最小構成で開始し、必要に応じて拡張する。

-   Cloud Run のリクエストログ / エラーログ（admin / イベント処理ワーカー / STG reader）
-   Pub/Sub のメッセージ配信ログ
-   Cloud Build のビルドログ
-   Cloudflare Workers のログ（Wrangler tail / Workers Analytics）— PRD reader

------------------------------------------------------------------------

## 12. Terraform構成案

```
modules/
  ├── artifact_registry
  ├── cloudrun_service
  ├── eventarc_trigger
  ├── firebase_storage
  ├── firestore
  ├── iam
  ├── pubsub
  └── secret_manager

environments/
  ├── stg/
  └── prd/
```

Terraform管理外のコンポーネント：

| コンポーネント | 管理方法 | 理由 |
|---|---|---|
| Cloud Build | GCPコンソール / gcloud CLI | ビルドトリガーはリポジトリ連携が必要であり、初期セットアップ後の変更頻度が低いため |
| Cloudflare Workers | Wrangler CLI（CI/CDパイプライン内） | OpenNextビルド成果物のデプロイはWranglerが担当するため |
| Cloudflare DNS | Cloudflareダッシュボード | 独自ドメイン取得後にTerraform管理に移行予定 |
| Cloudflare WAF | Cloudflareダッシュボード | 独自ドメイン取得後にTerraform管理に移行予定 |

------------------------------------------------------------------------

## 13. コスト概算

### 13.1 Cloudflare（PRD reader）

| サービス | 無料枠 | 超過時 |
|---|---|---|
| Workers | 100,000リクエスト/日 | $5/月で1,000万リクエスト |
| 帯域幅 | 無制限（$0） | - |
| DNS / TLS | 無料 | - |
| WAF | Freeプランで基本ルール | Proプラン $20/月 |

### 13.2 Google Cloud（STG/PRD共通）

| サービス | 無料枠 | 超過時の目安 |
|---|---|---|
| Cloud Run | vCPU 180,000秒/月、メモリ 360,000 GiB秒/月 | vCPU: $0.00002400/秒、メモリ: $0.00000250/GiB秒 |
| Firestore | 読み取り 50,000/日、書き込み 20,000/日、ストレージ 1GiB | 読み取り $0.036/10万、書き込み $0.108/10万 |
| Firebase Storage | ストレージ 5GB、ダウンロード 1GB/日 | ストレージ $0.026/GB、エグレス $0.12/GB |
| Pub/Sub | 10GB/月 | $40/TiB |
| Secret Manager | 6アクセス/月（無料） | $0.03/10,000アクセス |
| Artifact Registry | 500MB | $0.10/GB |

### 13.3 初期運用時の月額目安

小規模（DAU 数百人程度）の場合、**ほぼ無料枠内で運用可能**。
Cloud Run の scale-to-zero により、アイドル時のコストはゼロ。

------------------------------------------------------------------------

## 14. インフラ移行に伴う実装修正

reader のみが Workers に移行するため、影響範囲は限定的。
admin は STG/PRD ともに Cloud Run のため、既存コードの修正は不要。

### 14.1 影響範囲サマリ

| アプリ | PRDホスティング | firebase-admin | Workers移行の影響 |
|---|---|---|---|
| **reader** | Workers | 未使用 | ISR対応、Firebase client SDK の動作確認 |
| **admin** | Cloud Run | 使用 | **なし**（既存コードそのまま） |

### 14.2 必須修正（reader — OpenNext互換性）

#### ISR（Incremental Static Regeneration）

以下のファイルで `revalidate` セグメント設定を使用しているが、OpenNext ではISRの対応が限定的。
キャッシュ戦略の再設計が必要。

| ファイル | 設定 |
|---|---|
| `shared/src/components/organisms/article/index.tsx` | `revalidate = 3600` |
| `shared/src/components/organisms/memo/index.tsx` | `revalidate = 3600` |
| `shared/src/components/organisms/series/chapter/index.tsx` | `revalidate = 3600` |

対応方針：Cloudflare Cache API / KV を活用したキャッシュ戦略に再設計

#### Firebase client SDK の Workers 動作確認

reader は Firebase client SDK（`firebase/firestore`, `firebase/auth`, `firebase/storage`）を使用。
client SDK は `fetch` ベースで動作するため Workers 上で動作する見込みだが、以下の検証が必要：

-   `FirebaseProvider.firestore` の初期化と CRUD 操作
-   `runTransaction` の動作（`reader/src/app/api/engagement/route.ts` で使用）
-   `firebase/storage` の `getDownloadURL` 等の動作

### 14.3 推奨修正

| ファイル | 内容 | 理由 |
|---|---|---|
| `shared/src/actions/view.ts` | `import { randomUUID } from "crypto"` | Workers互換だが `crypto.randomUUID()` への変更を推奨 |

### 14.4 修正不要

#### reader の Server Actions（全て Firebase client SDK のみ使用）

| ファイル | 用途 |
|---|---|
| `shared/src/actions/article.ts` | 記事検索・取得 |
| `shared/src/actions/memo.ts` | メモ検索・取得 |
| `shared/src/actions/view.ts` | ページビュー記録 |
| `shared/src/actions/search-log.ts` | 検索ログ記録 |
| `shared/src/actions/ogp.ts` | OGPメタデータ取得 |
| `reader/src/app/api/engagement/route.ts` | エンゲージメント記録 API |

#### admin（全ファイル修正不要 — Cloud Run で稼働するため）

firebase-admin / @google-cloud/pubsub を含む全コードがそのまま動作する。

#### Infrastructure層

| ファイル | 用途 |
|---|---|
| `shared/src/providers/infrastructure/firebase.ts` | Firebase client SDK 初期化 |
| `shared/src/infrastructures/common.ts` | Firestore操作の抽象化層 |

### 14.5 環境変数の分配

#### Cloudflare Workers — reader PRD（Wrangler secrets）

| 変数名 | 用途 |
|---|---|
| `NEXT_PUBLIC_FIREBASE_API_KEY` | Firebase client SDK |
| `NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN` | Firebase Auth |
| `NEXT_PUBLIC_FIREBASE_PROJECT_ID` | Firebase プロジェクト |
| `NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET` | Firebase Storage |
| `NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID` | Firebase Messaging |
| `NEXT_PUBLIC_FIREBASE_APP_ID` | Firebase App ID |
| `NEXT_PUBLIC_SITE_URL` | サイトURL |

#### Cloud Run — admin PRD / 全STG / イベント処理ワーカー（Secret Manager）

| 変数名 | 用途 | 対象 |
|---|---|---|
| `NEXT_PUBLIC_FIREBASE_*` | Firebase client SDK | admin, reader(STG) |
| `FIREBASE_PROJECT_ID` | firebase-admin 初期化 | admin |
| `OIDC_ALLOWED_EMAILS` | 管理者メールリスト | admin |
| `EVENT_PUBSUB_TOPIC_NAME` | Pub/Sub トピック名 | admin |
| `E2E_AUTH_ENABLED` | E2Eテスト有効化フラグ | admin |
| `E2E_AUTH_SECRET` | E2Eテスト認証シークレット | admin |

### 14.6 移行フェーズ

#### Phase 1: OpenNext 対応（reader）

1.  OpenNext でのビルド・動作検証
2.  Firebase client SDK の Workers 上での動作確認
3.  ISR のキャッシュ戦略を再設計（Cloudflare Cache API / KV 活用）

#### Phase 2: Workers デプロイ（reader PRD）

1.  Wrangler 設定ファイル作成
2.  Wrangler secrets に環境変数を設定
3.  Workers へデプロイ、動作確認

#### Phase 3: CI/CD パイプライン構築

1.  GitHub Actions でのテスト自動化
2.  Cloud Build パイプライン（admin / イベント処理ワーカー / STG reader）
3.  Wrangler デプロイパイプライン（PRD reader）

------------------------------------------------------------------------

## 15. 将来の拡張

### 15.1 独自ドメイン取得

必要になったタイミングでCloudflare Registrarから取得し、以下を設定：

-   Cloudflare DNS でドメイン管理
-   PRD reader: Workers カスタムドメイン設定
-   PRD admin: Cloudflare → Cloud Run のリバースプロキシ設定
-   STG: サブドメインで分離
-   WAF / Rate Limit 有効化（PRD）
-   画像CDN（Cloudflare プロキシ経由でFirebase Storage配信）

### 15.2 admin の Workers 移行

Firebase公式がedge runtime対応（Firebase Admin Lite等）をリリースした場合、
admin も Workers に移行可能。現時点では対応予定なし。

### 15.3 STGの Workers 移行

将来的にSTGの reader もCloudflare Workersに統一可能。
PRDでの運用が安定したら検討する。

------------------------------------------------------------------------

## 16. まとめ

-   PRD reader はCloudflare Workers でエッジ配信（firebase-admin 不使用のため問題なし）
-   PRD admin / イベント処理ワーカーは Cloud Run（firebase-admin フル活用）
-   STGは全サービス Cloud Run で統合
-   無料URLで開始し、独自ドメインは後回し
-   イベント駆動で拡張性あり（ワーカー分離 + ドメイン単位トピック）
-   Secret管理が安全（環境ごとに適切な方法で注入）
-   Firebase Storage + Security Rulesで画像管理（初期はCDNなし）
-   GitHub Actions + Cloud Build + Wranglerで柔軟なCI/CD
-   初期は無料枠内で運用可能
-   実装修正は reader の OpenNext 対応（ISR、Firebase client SDK 動作確認）のみ

堅牢かつ将来拡張可能なインフラ構成である。
