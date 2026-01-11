import { Career } from "@shared/domains/user";
import styles from "./career.module.css";
import { VerticalLine } from "@shared/components/atoms/line/vertical";
import { CareerCard } from "./card/career";
import { SectionCard } from "@shared/components/atoms/card/section";

export type Props = {
  careers: Career[];
};

export const CareerList = (props: Props) => (
  <SectionCard title="経歴">
    <div className={styles.cards}>
      <div className={styles.line}>
        <VerticalLine />
      </div>
      {props.careers.map((career, index) => (
        <div key={index} className={styles.item}>
          <div className={styles.point} />
          <div className={styles.card}>
            <CareerCard career={career} />
          </div>
        </div>
      ))}
    </div>
  </SectionCard>
);
