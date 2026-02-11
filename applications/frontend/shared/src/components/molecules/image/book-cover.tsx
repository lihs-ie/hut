import { CommonImage } from "@shared/components/atoms/image/common";
import styles from "./book-cover.module.css";
import { ModestText } from "@shared/components/atoms/text/modest";
import { formatDate } from "@shared/aspects/date";

export type Props = {
  src: string;
  alt: string;
  createdAt: Date;
};

export const BookCover = (props: Props) => (
  <div className={styles.container}>
    <div className={styles.image}>
      <CommonImage src={props.src} alt={props.alt} />
    </div>

    <hr className={styles.separator} />

    <ModestText>{`公開：${formatDate(props.createdAt)}`}</ModestText>
  </div>
);
