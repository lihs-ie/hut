import { SectionCard } from "@shared/components/atoms/card/section";
import { TechStackCard } from "./card/tech-stack";
import styles from "./tech-stack.module.css";

export type Props = {
  techStacks: Map<string, { from: Date; name: string; logo: string }[]>;
  now: Date;
};

export const TechStackList = (props: Props) => (
  <SectionCard title="技術スタック">
    <div className={styles.items}>
      {Array.from(props.techStacks.values())
        .flat()
        .map((stack, index) => (
          <TechStackCard
            key={index}
            from={stack.from}
            now={props.now}
            techStackName={stack.name}
            techStackLogo={stack.logo}
          />
        ))}
    </div>
  </SectionCard>
);
