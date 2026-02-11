import { PrivacyPolicy } from "@shared/domains/document";
import styles from "./privacy.module.css";
import { SimpleCard } from "@shared/components/atoms/card/simple";

export type Props = {
  privacy: PrivacyPolicy;
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
    </article>
  </SimpleCard>
);
