# 課金アラート受信時の手動シャットダウン Runbook

GCP 予算アラート（`billing_budget` モジュールが配信）を受け取ったときに、
段階的に課金を抑える手順をまとめたものです。Issue #72 の縮小スコープ（案 A）に基づき、
自動シャットダウンは行わず、人手で確実に止めることを前提とします。

対象環境: stg（月次予算 ¥3,000）/ prd（月次予算 ¥10,000）

しきい値: `0.5`, `0.8`, `1.0`, `1.2`（50/80/100/120 %）。
通知チャネルはメール 1 つで、stg/prd で同じ送信先を共有することを想定しています。

## 事前準備: 環境変数の設定

この Runbook 内のコマンド例はすべて環境変数を参照しています。実作業に入る前にシェルで以下を設定してください。
値は 1Password などのシークレットストアや `gh secret list --env stg-plan` / `gh secret list --env prd-plan` で確認できるものを使います。

```bash
export STG_PROJECT_ID=<GCP stg project id>
export PRD_PROJECT_ID=<GCP prd project id>
export BILLING_ACCOUNT_ID=<GCP billing account id>
export OWNER_EMAIL=<リポジトリ所有者のメールアドレス>
```

> 緊急度の早見表
>
> - 50 % 通知: 観測のみ。Section 1 の確認を実施。
> - 80 % 通知: 残予算と直近の変更を確認。Section 1 + Section 2 の準備。
> - 100 % 通知: Section 2 を実行（Workers/Cloud Run の停止）。
> - 120 % 通知: Section 3 を実行（Firestore rules を全拒否化）。

---

## Section 1: アラート受信時の確認手順

1. **GCP コンソールで実費の内訳を見る**

   - prd: <https://console.cloud.google.com/billing/${BILLING_ACCOUNT_ID}/reports?project=${PRD_PROJECT_ID}>
   - stg: <https://console.cloud.google.com/billing/${BILLING_ACCOUNT_ID}/reports?project=${STG_PROJECT_ID}>

   ※ URL の billing account ID は実環境のものに置き換えてください。

2. **どのサービスが急増しているかを「Group by service」で確認**

   - `Cloud Firestore`、`Cloud Run`、`Cloud Storage`、`Networking` の上位を確認。

3. **Firestore 使用量ダッシュボードを開く**

   - prd: <https://console.cloud.google.com/firestore/databases/-default-/usage?project=${PRD_PROJECT_ID}>
   - 直近の Read / Write / Storage 量を確認。
   - スパイクがあれば、原因となったコレクション（`articles`, `search-tokens` など）を特定。

4. **Cloud Run のリクエスト数とエラー率を確認**

   - prd admin: <https://console.cloud.google.com/run/detail/asia-northeast1/${PRD_PROJECT_ID}-admin/metrics?project=${PRD_PROJECT_ID}>
   - stg admin: <https://console.cloud.google.com/run/detail/asia-northeast1/${STG_PROJECT_ID}-admin/metrics?project=${STG_PROJECT_ID}>
   - stg reader: <https://console.cloud.google.com/run/detail/asia-northeast1/${STG_PROJECT_ID}-reader/metrics?project=${STG_PROJECT_ID}>
   - 1 分あたりリクエスト数が普段より 1 桁以上多ければ、スクレイパや誤設定のジョブを疑う。

5. **Cloudflare ダッシュボードでアクセスログ / ボット検知を確認**（prd reader）

   - <https://dash.cloudflare.com> → 対象 Worker → Analytics & Logs。
   - WAF / Bot Analytics で異常な国・User-Agent を確認。

6. **直近のデプロイを確認**

   - `git log --since='24 hours ago' main` で最近マージされた変更を確認。
   - 想定外のキャッシュ無効化や N+1 クエリが入っていないかをコードで確認。

これらで原因が掴めて自然減が見込めるなら、ここで停止せず観測を続けます。
収束しない／120 % に到達した場合は Section 2 へ進みます。

---

## Section 2: prd reader（Cloudflare Workers）の手動停止手順

prd reader は Cloudflare Workers にデプロイされた静的サイトです。
GCP 側のリソースではないため、Cloudflare ダッシュボードから切り離します。

1. <https://dash.cloudflare.com> にログイン。
2. 左メニュー **Workers & Pages** を開く。
3. 対象の Worker / Pages プロジェクト（reader 本番）を選択。
4. **Settings** → **Triggers** → **Routes** を開く。
5. 本番ドメイン（例: `hut.example.com/*`）の Route を **Delete** する。
   - 完全に削除する代わりに、一時的にダミードメインへ差し替えるのも可。
6. **Custom Domains** が有効な場合は、対応するドメインを **Disable** に切り替える。
7. ブラウザで対象 URL にアクセスし、Cloudflare の標準 522 / DNS error が返ることを確認。

> 注意: Worker そのもの（コード）を削除する必要はありません。Routes を外すだけで配信が止まります。
> 復旧時に Routes を貼り直せば即時復帰できます。

並行して、必要であれば **prd admin / search-token-worker** も止めます。
これらは Cloud Run なので以下のコマンドで `min-instances`/`max-instances` を 0 にします。

```bash
gcloud run services update ${PRD_PROJECT_ID}-admin \
  --region=asia-northeast1 \
  --project=${PRD_PROJECT_ID} \
  --min-instances=0 \
  --max-instances=0

gcloud run services update search-token-worker \
  --region=asia-northeast1 \
  --project=${PRD_PROJECT_ID} \
  --min-instances=0 \
  --max-instances=0

gcloud run services update image-cleanup-worker \
  --region=asia-northeast1 \
  --project=${PRD_PROJECT_ID} \
  --min-instances=0 \
  --max-instances=0
```

`max-instances=0` にすると新規リクエストが即時拒否され、課金対象のインスタンスが立たなくなります。
イメージや revision はそのまま残るため、復旧時は同じコマンドで `--max-instances=100`（既定値）に戻すだけで配信が再開します。

---

## Section 3: Firestore rules を一時的に全拒否版へ差し替える手順

Firestore の Read / Write が原因で予算を超過しているケースの最終手段です。
すべてのアクセスを deny に倒し、API 呼び出し数自体をゼロに近づけます。

1. 退避: 現行ルールをタイムスタンプ付きで控える。

   ```bash
   cp firestore.rules firestore.rules.backup-$(date +%Y%m%d-%H%M%S)
   ```

2. 差し替え: `firestore.rules` を以下の全拒否版に置き換える。

   ```firestore-rules
   rules_version = '2';
   service cloud.firestore {
     match /databases/{database}/documents {
       match /{document=**} {
         allow read, write: if false;
       }
     }
   }
   ```

3. デプロイ:

   ```bash
   firebase deploy --only firestore:rules --project ${PRD_PROJECT_ID}
   firebase deploy --only firestore:rules --project ${STG_PROJECT_ID}
   ```

4. 確認:

   - Firestore コンソールの「Rules」タブで全拒否ルールが反映されたことを確認。
   - prd admin の API レスポンスが 403 / permission-denied になることをサンプル確認。

> 注意: この状態では reader / admin 共に一切の読み書きができません。
> サイト全体が事実上停止するため、Section 2 の Cloudflare Routes 切断と同時に行ってください。

---

## Section 4: 復旧手順（停止解除の逆手順）

原因の修正および予算上限の見直し（必要なら `budget_amount_jpy` の引き上げ PR）を完了した後、
以下の順に逆操作で復旧します。

1. **Firestore rules を元に戻す**

   ```bash
   cp firestore.rules.backup-YYYYMMDD-HHMMSS firestore.rules
   firebase deploy --only firestore:rules --project ${PRD_PROJECT_ID}
   firebase deploy --only firestore:rules --project ${STG_PROJECT_ID}
   ```

   - デプロイ後、admin から 1 件 read / write して 200 が返ることを確認。

2. **Cloud Run の max-instances を戻す**

   ```bash
   gcloud run services update ${PRD_PROJECT_ID}-admin \
     --region=asia-northeast1 \
     --project=${PRD_PROJECT_ID} \
     --min-instances=0 \
     --max-instances=100

   gcloud run services update search-token-worker \
     --region=asia-northeast1 \
     --project=${PRD_PROJECT_ID} \
     --min-instances=0 \
     --max-instances=100

   gcloud run services update image-cleanup-worker \
     --region=asia-northeast1 \
     --project=${PRD_PROJECT_ID} \
     --min-instances=0 \
     --max-instances=100
   ```

3. **Cloudflare Workers の Routes を再作成**

   - Workers & Pages → 対象プロジェクト → Settings → Triggers → Routes → **Add route**。
   - 元のドメインパターンを設定（例: `hut.example.com/*`）。
   - Custom Domains を Disable していた場合は再度 Enable。

4. **動作確認**

   - 本番 URL を開いてトップページが描画されることを確認。
   - 主要記事ページ・検索ページが描画されることを確認。
   - admin にログインし、記事の一覧取得・編集を 1 件試す。

5. **事後対応**

   - 課金ダッシュボードを 30 分後・1 時間後・翌朝に再確認し、再発していないか監視。
   - 原因と再発防止策を `docs/internal/runbooks/billing-shutdown.md` の末尾、
     または別途 incident note にメモ。

---

## Section 5: 連絡先・エスカレーション

- **一次対応**: リポジトリ所有者（`${OWNER_EMAIL}`）
- **連絡手段**: GitHub Issue（`infra` ラベル）/ メール
- **GCP サポート**: 個人プロジェクトのため Basic サポートのみ。
  クォータ調整など Google 側の操作が必要な場合は GCP コンソールから Support ケースを起票。
- **Cloudflare サポート**: Free プランのため Community サポートのみ。Routes / DNS のセルフサービスで完結する範囲で運用する。

> このドキュメントは Issue #72（縮小スコープ案 A）に対応します。
> 自動シャットダウン（Cloud Functions などによる自動 Workers 停止）は実装していないため、
> 本 Runbook の手動手順が唯一の停止方法です。
