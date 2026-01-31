import { SimpleCard } from "@shared/components/atoms/card/simple";
import { SectionHeader } from "@shared/components/molecules/form/section-header";
import { TechStackItem } from "@shared/components/molecules/form/tech-stack";
import styles from "./tech-stack.module.css";
import { AddButton } from "@shared/components/molecules/button/add";
import { UnvalidatedTechnologyStack } from "@shared/domains/common/tech";
import { Tag } from "@shared/domains/attributes/tag";

export type Props = {
  values: UnvalidatedTechnologyStack[];
  tags: Tag[];
  onAdd: () => void;
  onUpdate: (index: number, value: UnvalidatedTechnologyStack) => void;
  onRemove: (index: number) => void;
};

export const ProfileTechStackForm = (props: Props) => {
  const usedTagIdentifiers = props.values
    .map((tech) => tech.tag)
    .filter(Boolean);

  const getAvailableTags = (currentTagIdentifier: string) => {
    return props.tags.filter(
      (tag) =>
        !usedTagIdentifiers.includes(tag.identifier) ||
        tag.identifier === currentTagIdentifier,
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
            key={value.tag || `new-${index}`}
            value={value}
            index={index}
            onUpdate={(index, value) => props.onUpdate(index, value)}
            onRemove={props.onRemove}
            availableTags={getAvailableTags(value.tag)}
          />
        ))}
      </div>
    </SimpleCard>
  );
};
