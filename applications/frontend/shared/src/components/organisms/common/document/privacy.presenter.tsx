import { PrivacyPolicy } from "@shared/domains/document";
import styles from "./privacy.module.css";
import { SimpleCard } from "@shared/components/atoms/card/simple";

export type Props = {
  privacy: PrivacyPolicy;
};

const SENTRY_NOTICE = {
  headline: "エラー監視ツールの利用について",
  body: "本サービスでは、アプリケーションの品質向上およびユーザー体験改善のためにエラー監視サービス Sentry (Functional Software, Inc.) を利用しています。",
  list: [
    "送信データ: エラー発生時のスタックトレース、ブラウザ・OS 情報、URL、匿名化されたリクエストヘッダー、Web Vitals (Core Web Vitals) などのパフォーマンス指標",
    "送信しないデータ: メールアドレス、パスワード、認証トークン、IP アドレスを含む個人を特定し得る情報はフィルタリング機構によって送信前に除去または [REDACTED] に置換されます",
    "保存期間: Sentry の標準保存期間である 90 日以内に削除されます",
    "利用目的: 不具合の早期検知、原因調査、サービス品質とパフォーマンスの継続的改善",
  ],
};

export const PrivacySectionsPresenter = (props: Props) => (
  <SimpleCard>
    <article className={styles.container}>
      {props.privacy.sections.map((section, index) => (
        <section key={index} className={styles.section}>
          <h2 className={styles.title}>{section.headline}</h2>
          <div className={styles.content}>
            <p>{section.body}</p>
            <ul className={styles.list}>
              {section.list &&
                section.list.map((item, index) => <li key={index}>{item}</li>)}
            </ul>
          </div>
        </section>
      ))}
      <section className={styles.section}>
        <h2 className={styles.title}>{SENTRY_NOTICE.headline}</h2>
        <div className={styles.content}>
          <p>{SENTRY_NOTICE.body}</p>
          <ul className={styles.list}>
            {SENTRY_NOTICE.list.map((item, index) => (
              <li key={index}>{item}</li>
            ))}
          </ul>
        </div>
      </section>
    </article>
  </SimpleCard>
);
