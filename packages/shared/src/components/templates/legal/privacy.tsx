import { PrivacyPolicy } from "@shared/domains/document";
import styles from "./privacy.module.css";
import { PrivacySections } from "@shared/components/organisms/common/document/privacy";

export type Props = {
  getPrivacy: () => Promise<PrivacyPolicy>;
};

export const revalidate = false;

export const PrivacyIndex = (props: Props) => (
  <article className={styles.container}>
    <h1 className={styles.title}>プライバシーポリシー</h1>
    <PrivacySections getPrivacy={props.getPrivacy} />
  </article>
);
