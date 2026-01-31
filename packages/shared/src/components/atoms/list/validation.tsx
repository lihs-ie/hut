import { ValidationError } from "@shared/aspects/error";
import styles from "./validation.module.css";

export type Props = {
  errors: ValidationError[];
};

export const ValidationErrorList = (props: Props) => (
  <ul className={styles.container}>
    {props.errors.map((error, index) => (
      <li key={index} className={styles.error}>
        {error.field}：{error.description}
      </li>
    ))}
  </ul>
);
