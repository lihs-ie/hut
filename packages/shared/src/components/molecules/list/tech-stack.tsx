import { SectionCard } from "@shared/components/atoms/card/section";
import { TechStackCard } from "./card/tech-stack";
import styles from "./tech-stack.module.css";
import {
  TechnologyCategory,
  TechnologyKind,
  TechnologyStack,
} from "@shared/domains/common/tech";

export type Props = {
  techStacks: Map<TechnologyCategory, TechnologyStack[]>;
  now: Date;
  logoSources: Record<TechnologyKind, string>;
};

export const TechStackList = (props: Props) => (
  <SectionCard title="技術スタック">
    <div className={styles.list}>
      {Array.from(props.techStacks.entries()).map(([category, techStacks]) => (
        <div key={category} className={styles.items}>
          <h3 className={styles.category}>{category.toUpperCase()}</h3>

          {techStacks.map((techStack) => (
            <TechStackCard
              key={techStack.kind}
              techStack={techStack}
              now={props.now}
              logoSources={props.logoSources}
            />
          ))}
        </div>
      ))}
    </div>
  </SectionCard>
);
