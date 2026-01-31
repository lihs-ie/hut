import styles from "./error.module.css";

type Props = {
  code: string;
  title: string;
  message: string;
};

export const ErrorMessage = (props: Props) => (
  <div className={styles.container}>
    <div className={styles.code}>{props.code}</div>
    <h1 className={styles.title}>{props.title}</h1>
    <p className={styles.message}>{props.message}</p>
  </div>
);
