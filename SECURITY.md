# Security Policy / セキュリティポリシー

## Supported Versions / サポート対象バージョン

本プロジェクトでは、セキュリティ修正は下記のブランチ・リリースに対してのみ提供します。

This project provides security fixes only for the following branches and releases:

| Version                | Supported |
| ---------------------- | --------- |
| `main` (latest stable) | Yes       |
| Latest release         | Yes       |
| Older releases         | No        |

古いバージョンでセキュリティ問題が発見された場合でも、原則として `main` および最新リリースに対してのみ修正を適用します。

For older versions, security issues are generally addressed only on `main` and the latest release.

## Reporting a Vulnerability / 脆弱性の報告

**Public Issue で脆弱性を報告しないでください。**
**Please do not report vulnerabilities via public GitHub Issues.**

脆弱性を発見した場合は、GitHub の **Private vulnerability reporting** 機能を利用して報告してください。

When you discover a vulnerability, please report it via GitHub's **Private vulnerability reporting** feature.

1. 本リポジトリの [Security タブ](https://github.com/lihs-ie/hut/security) を開く
2. 左メニューの **"Advisories"** → **"Report a vulnerability"** をクリック
3. 以下の情報を含めて送信する
   - 影響を受けるコンポーネント / 機能
   - 再現手順 (可能であれば最小限の PoC)
   - 推定される影響範囲・深刻度 (CVSS が算出可能であれば併記)
   - 発見者の連絡先 (任意)

---

1. Open the [Security tab](https://github.com/lihs-ie/hut/security) of this repository.
2. In the left menu, click **"Advisories"** → **"Report a vulnerability"**.
3. Submit a report including:
   - Affected component / feature
   - Reproduction steps (a minimal PoC if possible)
   - Estimated impact and severity (include a CVSS score if available)
   - Contact information (optional)

## Response SLA / 対応 SLA

| フェーズ / Phase                               | 目標期間 / Target        |
| --------------------------------------------- | ----------------------- |
| 受領確認 / Acknowledgement                     | 3 営業日以内 / 3 business days |
| 初期トリアージ / Initial triage                | 7 営業日以内 / 7 business days |
| 修正方針の共有 / Share remediation plan        | トリアージ完了後、順次 / After triage, on a rolling basis |
| 修正リリース / Fix release                     | 深刻度に応じ個別調整 / Depends on severity |

- **Critical / High**: 最優先で対応し、可能な限り速やかにパッチを提供します。
- **Medium / Low**: 次回リリースサイクルで対応することがあります。

---

- **Critical / High**: Handled with top priority; a patch will be released as soon as possible.
- **Medium / Low**: May be bundled into the next regular release cycle.

## Disclosure Policy / 開示方針

本プロジェクトは **責任ある開示 (Coordinated / Responsible Disclosure)** を採用します。

This project follows a **Coordinated / Responsible Disclosure** policy.

- 報告者と合意した修正リリース後に、GitHub Security Advisory として公開します。
- 公開前に修正リリースが完了していない場合、公開日は再調整します。
- 報告者をクレジットとして掲載することが可能です (希望者のみ)。

---

- We publish a GitHub Security Advisory **after** a fix has been released and coordinated with the reporter.
- If a fix cannot be released before the planned disclosure date, the disclosure date will be rescheduled.
- Reporters may be credited in the advisory upon request.

## Contact / 連絡先

脆弱性報告は **GitHub Security Advisory (Private vulnerability reporting)** 経由を第一に受け付けます。
その他の問い合わせ経路は提供していません。

The primary channel for vulnerability reports is **GitHub Security Advisory (Private vulnerability reporting)**.
No other contact channels are provided for vulnerability reports.
