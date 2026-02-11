import { FormLabel } from "@shared/components/atoms/text/form-label";
import { MonthInput } from "@shared/components/atoms/input/month";
import { Checkbox } from "@shared/components/atoms/input/checkbox";
import styles from "./tech-stack.module.css";
import { RemoveButton } from "../button/remove";
import { SimpleSelect } from "@shared/components/atoms/select/simple";
import { UnvalidatedTechnologyStack } from "@shared/domains/common/tech";
import { Tag } from "@shared/domains/attributes/tag";

export type Props = {
  value: UnvalidatedTechnologyStack;
  index: number;
  onUpdate: (index: number, value: UnvalidatedTechnologyStack) => void;
  onRemove: (index: number) => void;
  availableTags: Tag[];
};

export const TechStackItem = (props: Props) => (
  <div className={styles.container}>
    <div className={styles.first}>
      <div className={styles.fields}>
        <div className={styles.field}>
          <FormLabel>技術</FormLabel>
          <SimpleSelect
            value={props.value.tag}
            onChange={(value) =>
              props.onUpdate(props.index, {
                ...props.value,
                tag: value,
              })
            }
          >
            <option value="">選択してください</option>
            {props.availableTags.map((tag) => (
              <option key={tag.identifier} value={tag.identifier}>
                {tag.name}
              </option>
            ))}
          </SimpleSelect>
        </div>

        <div className={styles.field}>
          <FormLabel>開始日</FormLabel>
          <MonthInput
            value={props.value.from.toISOString().slice(0, 7)}
            onChange={(value) =>
              props.onUpdate(props.index, { ...props.value, from: value })
            }
          />
        </div>

        <div className={styles.field}>
          <FormLabel>経験種別</FormLabel>
          <SimpleSelect
            value={props.value.type}
            onChange={(value) =>
              props.onUpdate(props.index, {
                ...props.value,
                type: value,
              })
            }
          >
            <option value="personal">個人</option>
            <option value="professional">業務</option>
            <option value="both">両方</option>
          </SimpleSelect>
        </div>
      </div>

      <RemoveButton
        ariaLabel="削除"
        onClick={() => props.onRemove(props.index)}
      />
    </div>

    <div className={styles.fields}>
      <Checkbox
        checked={props.value.continue}
        onChange={(value) => {
          props.onUpdate(props.index, {
            ...props.value,
            continue: value,
          });
        }}
        label="現在も使用中"
      />
    </div>
  </div>
);
