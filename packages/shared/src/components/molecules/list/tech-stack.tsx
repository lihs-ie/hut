import { SectionCard } from "@shared/components/atoms/card/section";
import { TechStackCard } from "./card/tech-stack";
import styles from "./tech-stack.module.css";

export type Props = {
  techStacks: Map<string, { from: Date; name: string; logo: string }[]>;
  now: Date;
};

export const TechStackList = (props: Props) => (
  <SectionCard title="技術スタック">
    <div className={styles.list}>
      {Array.from(props.techStacks.entries()).map(([category, stacks]) => (
        <div key={category} className={styles.items}>
          <h3 className={styles.category}>{category.toUpperCase()}</h3>

          {stacks.map((stack, index) => (
            <TechStackCard
              key={index}
              from={stack.from}
              now={props.now}
              techStackName={stack.name}
              techStackLogo={stack.logo}
            />
          ))}
        </div>
      ))}
    </div>
  </SectionCard>
);
