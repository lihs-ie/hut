# billing_budget

`google_billing_budget` と `google_monitoring_notification_channel` を組み合わせて、
特定の GCP プロジェクトの月次コストがしきい値を超えたらメール通知を送るモジュールです。

Issue #72 の縮小スコープ（予算アラート通知まで）を実現します。Workers / Firestore の
自動シャットダウンは行いません。アラート受信後の手動停止手順は
`docs/internal/runbooks/billing-shutdown.md` を参照してください。

## リソース

- `google_monitoring_notification_channel.email` — 通知先メールのチャネル
- `google_billing_budget.this` — プロジェクト単位の月次予算と複数しきい値アラート

## 変数

| 名前 | 型 | 必須 | デフォルト | 説明 |
|------|----|------|------------|------|
| `billing_account_id` | `string` | はい | — | GCP 請求先アカウント ID（例: `01ABCD-234567-89EFGH`） |
| `display_name` | `string` | はい | — | GCP コンソールに表示される予算名 |
| `budget_amount_jpy` | `number` | はい | — | 月額予算（日本円、整数） |
| `notification_email` | `string` | はい | — | アラート通知の送信先メールアドレス（sensitive） |
| `project_id` | `string` | はい | — | 監視対象の GCP プロジェクト ID |
| `threshold_percents` | `list(number)` | いいえ | `[0.5, 0.8, 1.0, 1.2]` | アラートを発火させる予算しきい値（割合） |
| `labels` | `map(string)` | いいえ | `{}` | 通知チャネルに付与するラベル |

## 出力

| 名前 | 説明 |
|------|------|
| `budget_name` | 作成した予算の完全修飾リソース名 |
| `notification_channel_id` | 作成した通知チャネルの ID |

## 使用例

```hcl
module "billing_budget" {
  source = "../../modules/billing_budget"

  project_id         = var.project_id
  billing_account_id = var.billing_account_id
  display_name       = "${var.project_id} monthly budget"
  budget_amount_jpy  = 3000
  notification_email = var.notification_email

  labels = local.common_labels
}
```

## 前提条件

- `cloudbilling.googleapis.com` が有効化されていること
- `monitoring.googleapis.com` が有効化されていること
- 実行する Terraform サービスアカウントに
  `roles/billing.costsManager`（または同等）が付与されていること
- `notification_email` に指定したアドレスの受信者が
  GCP Monitoring からの検証メールを承認済みであること（初回のみ）
