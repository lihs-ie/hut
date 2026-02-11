import { CommonImage } from "@shared/components/atoms/image/common";
import styles from "./tech-stack.module.css";
import { determineExperience } from "@shared/aspects/date";

export type Props = {
  from: Date;
  now: Date;
  techStackName: string;
  techStackLogo: string;
};

export const TechStackCard = (props: Props) => (
  <div className={styles.container}>
    <div className={styles.logo}>
      <CommonImage src={props.techStackLogo} alt={props.techStackName} />
    </div>
    <div className={styles.body}>
      <span className={styles.type}>{props.techStackName}</span>
      <p className={styles.experience}>
        {determineExperience(props.from, props.now)}&nbsp;年
      </p>
    </div>
  </div>
);
