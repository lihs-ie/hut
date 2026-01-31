import type { DragEvent } from "react";
import { SimpleCard } from "@shared/components/atoms/card/simple";
import { PlusIcon } from "@shared/components/atoms/icon";
import { SectionHeader } from "@shared/components/molecules/form/section-header";
import { CareerItem } from "@shared/components/molecules/form/career";
import styles from "./career.module.css";
import { UnvalidatedCareer } from "@shared/domains/user";

export type Props = {
  careers: UnvalidatedCareer[];
  onAdd: () => void;
  onUpdate: (index: number, value: UnvalidatedCareer) => void;
  onRemove: (index: number) => void;
  onReorder: (fromIndex: number, toIndex: number) => void;
};

export const ProfileCareerForm = (props: Props) => {
  const handleDragStart = (event: DragEvent, index: number) => {
    event.dataTransfer.effectAllowed = "move";
    event.dataTransfer.setData("text/plain", index.toString());
  };

  const handleDragOver = (event: DragEvent) => {
    event.preventDefault();
    event.dataTransfer.dropEffect = "move";
  };

  const handleDrop = (event: DragEvent, dropIndex: number) => {
    event.preventDefault();
    const dragIndex = Number.parseInt(event.dataTransfer.getData("text/plain"));

    if (dragIndex !== dropIndex) {
      props.onReorder(dragIndex, dropIndex);
    }
  };

  return (
    <SimpleCard className={styles.container}>
      <SectionHeader title="経歴">
        <button type="button" onClick={props.onAdd} className={styles.add}>
          <PlusIcon className={styles.icon} />
          追加
        </button>
      </SectionHeader>

      <div className={styles.list}>
        {props.careers.map((career, index) => (
          <CareerItem
            key={`career-${index}`}
            value={career}
            index={index}
            onUpdate={props.onUpdate}
            onRemove={props.onRemove}
            onDragStart={handleDragStart}
            onDragOver={handleDragOver}
            onDrop={handleDrop}
          />
        ))}
      </div>
    </SimpleCard>
  );
};
