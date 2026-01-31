import { SimpleCard } from "@shared/components/atoms/card/simple";
import { SectionHeader } from "@shared/components/molecules/form/section-header";
import { TechStackItem } from "@shared/components/molecules/form/tech-stack";
import styles from "./tech-stack.module.css";
import { AddButton } from "@shared/components/molecules/button/add";
import { UnvalidatedTechnologyStack } from "@shared/domains/common/tech";

const TECH_OPTIONS = [
  "React",
  "Next.js",
  "TypeScript",
  "JavaScript",
  "Node.js",
  "Go",
  "Python",
  "Rust",
  "Vue.js",
  "Angular",
  "Svelte",
  "PostgreSQL",
  "MySQL",
  "MongoDB",
  "Redis",
  "Docker",
  "Kubernetes",
  "AWS",
  "GCP",
  "Azure",
  "Tailwind CSS",
  "styled-components",
];

export type Props = {
  values: UnvalidatedTechnologyStack[];
  onAdd: () => void;
  onUpdate: (index: number, value: UnvalidatedTechnologyStack) => void;
  onRemove: (index: number) => void;
};

export const ProfileTechStackForm = (props: Props) => {
  const usedTechnologies = props.values
    .map((tech) => tech.kind)
    .filter(Boolean);

  const getAvailableTechnologies = (currentTechnology: string) => {
    return TECH_OPTIONS.filter(
      (option) =>
        !usedTechnologies.includes(option) || option === currentTechnology,
    );
  };

  return (
    <SimpleCard className={styles.container}>
      <SectionHeader title="技術スタック">
        <AddButton hasLabel={true} onClick={props.onAdd} />
      </SectionHeader>

      <div className={styles.list}>
        {props.values.map((value, index) => (
          <TechStackItem
            key={value.kind}
            value={value}
            index={index}
            onUpdate={(index, value) => props.onUpdate(index, value)}
            onRemove={props.onRemove}
            availableTechnologies={getAvailableTechnologies(value.kind)}
          />
        ))}
      </div>
    </SimpleCard>
  );
};
