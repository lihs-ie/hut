import styles from "./content.module.css";

export type Props = {
  title: string;
  description: string;
  icon?: React.ReactNode;
};

export const ContentEmpty = (props: Props) => (
  <div className={styles.container}>
    {props.icon && <div className={styles.icon}>{props.icon}</div>}
    <h3 className={styles.title}>{props.title}</h3>
    <p className={styles.description}>{props.description}</p>
  </div>
);
