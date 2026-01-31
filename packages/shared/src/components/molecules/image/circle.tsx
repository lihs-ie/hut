import { CommonImage } from "@shared/components/atoms/image/common";
import styles from "./circle.module.css";

export type Props = {
  src: string;
  alt: string;
};

export const CircleImage = (props: Props) => (
  <div className={styles.container}>
    <CommonImage {...props} />
  </div>
);
