import type { DragEvent } from "react";
import { DragHandleIcon } from "@shared/components/atoms/icon/drag-handle";
import { FormLabel } from "@shared/components/atoms/text/form-label";
import { TextInput } from "@shared/components/atoms/input/text";
import { Textarea } from "@shared/components/atoms/input/textarea";
import { MonthInput } from "@shared/components/atoms/input/month";
import { FormFieldRow } from "./field-row";
import styles from "./career.module.css";
import { UnvalidatedCareer } from "@shared/domains/user";
import { RemoveButton } from "../button/remove";

export type Props = {
  value: UnvalidatedCareer;
  index: number;
  onUpdate: (index: number, value: UnvalidatedCareer) => void;
  onRemove: (index: number) => void;
  onDragStart: (event: DragEvent, index: number) => void;
  onDragOver: (event: DragEvent) => void;
  onDrop: (event: DragEvent, index: number) => void;
};

export const CareerItem = (props: Props) => (
  <div
    className={styles.container}
    draggable
    onDragStart={(event) => props.onDragStart(event, props.index)}
    onDragOver={props.onDragOver}
    onDrop={(event) => props.onDrop(event, props.index)}
  >
    <div className={styles.handle}>
      <DragHandleIcon />
    </div>

    <div className={styles.fields}>
      <div className={styles.field}>
        <FormLabel>企業名</FormLabel>
        <TextInput
          value={props.value.company}
          onChange={(value) =>
            props.onUpdate(props.index, { ...props.value, company: value })
          }
          placeholder="株式会社〇〇"
        />
      </div>

      <div className={styles.field}>
        <FormLabel>役割</FormLabel>
        <TextInput
          value={props.value.role}
          onChange={(value) =>
            props.onUpdate(props.index, { ...props.value, role: value })
          }
          placeholder="Senior Developer"
        />
      </div>

      <FormFieldRow>
        <div className={styles.field}>
          <FormLabel>開始日</FormLabel>
          <MonthInput
            value={props.value.period.from.toLocaleDateString("sv").slice(0, 7)}
            onChange={(value) =>
              props.onUpdate(props.index, {
                ...props.value,
                period: { ...props.value.period, from: value },
              })
            }
          />
        </div>

        <div className={styles.field}>
          <FormLabel>終了日（現在の場合は空欄）</FormLabel>
          <MonthInput
            value={
              props.value.period.to
                ? props.value.period.to.toLocaleDateString("sv").slice(0, 7)
                : ""
            }
            onChange={(value) =>
              props.onUpdate(props.index, {
                ...props.value,
                period: {
                  ...props.value.period,
                  to: value ? value : null,
                },
              })
            }
          />
        </div>
      </FormFieldRow>

      <div className={styles.field}>
        <FormLabel>業務内容</FormLabel>
        <Textarea
          value={props.value.description}
          onChange={(value) =>
            props.onUpdate(props.index, {
              ...props.value,
              description: value,
            })
          }
          placeholder="具体的な業務内容を記載"
          rows={3}
        />
      </div>
    </div>

    <RemoveButton onClick={() => props.onRemove(props.index)} />
  </div>
);
