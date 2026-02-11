import styles from "./section.module.css";

type Props = {
  title: string;
  description?: string;
};

export const SectionHeader = (props: Props) => (
  <div className={styles.container}>
    <h3 className={styles.title}>{props.title}</h3>
    {props.description && (
      <p className={styles.description}>{props.description}</p>
    )}
  </div>
);
