import { CommonImage } from "@shared/components/atoms/image/common";
import styles from "./tech-stack.module.css";
import { determineExperience } from "@shared/aspects/date";
import { TechnologyKind, TechnologyStack } from "@shared/domains/common/tech";

export type Props = {
  techStack: TechnologyStack;
  now: Date;
  logoSources: Record<TechnologyKind, string>;
};

export const TechStackCard = (props: Props) => (
  <div className={styles.container}>
    <div className={styles.logo}>
      <CommonImage
        src={props.logoSources[props.techStack.kind as TechnologyKind]}
        alt={props.techStack.kind}
      />
    </div>
    <div className={styles.body}>
      <span className={styles.type}>{props.techStack.kind}</span>
      <p className={styles.experience}>
        {determineExperience(props.techStack.from, props.now)}&nbsp;年
      </p>
    </div>
  </div>
);
