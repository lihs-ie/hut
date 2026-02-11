"use client";

import { Textarea } from "@shared/components/atoms/input/textarea";
import { TextInput } from "@shared/components/atoms/input/text";
import { VariantButton } from "@shared/components/atoms/button/variant";
import { Trash2Icon } from "@shared/components/atoms/icon/trash";
import styles from "./policy-section.module.css";

export type EditablePolicySection = {
  headline: string;
  body: string;
  list: string[] | null;
};

export type Props = {
  section: EditablePolicySection;
  onUpdate: (section: EditablePolicySection) => void;
  onDelete: () => void;
  showDelete?: boolean;
};

export const PolicySectionEditor = (props: Props) => {
  const handleAddItem = () => {
    props.onUpdate({
      ...props.section,
      list: [...(props.section.list || []), ""],
    });
  };

  const handleUpdateItem = (index: number, value: string) => {
    const newList = [...(props.section.list || [])];
    newList[index] = value;
    props.onUpdate({ ...props.section, list: newList });
  };

  const handleDeleteItem = (index: number) => {
    const newList = (props.section.list || []).filter((_, i) => i !== index);
    props.onUpdate({
      ...props.section,
      list: newList.length > 0 ? newList : null,
    });
  };

  const handleToggleList = () => {
    if (props.section.list) {
      props.onUpdate({ ...props.section, list: null });
    } else {
      props.onUpdate({ ...props.section, list: [] });
    }
  };

  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <TextInput
          value={props.section.headline}
          onChange={(value) =>
            props.onUpdate({ ...props.section, headline: value })
          }
          placeholder="セクションタイトル"
          className={styles["title-input"]}
        />
        {props.showDelete !== false && (
          <VariantButton
            variant="ghost"
            size="sm"
            onClick={props.onDelete}
            className={styles["delete-button"]}
          >
            <Trash2Icon className={styles.icon} />
          </VariantButton>
        )}
      </div>

      <Textarea
        value={props.section.body}
        onChange={(value) => props.onUpdate({ ...props.section, body: value })}
        placeholder="セクションの内容を入力してください"
        rows={4}
        className={styles["content-input"]}
      />

      {props.section.list && (
        <div className={styles["items-list"]}>
          <label className={styles["items-label"]}>リスト項目</label>
          {props.section.list.map((item, index) => (
            <div key={index} className={styles["item-row"]}>
              <TextInput
                value={item}
                onChange={(value) => handleUpdateItem(index, value)}
                placeholder="リスト項目"
                className={styles["item-input"]}
              />
              <VariantButton
                variant="ghost"
                size="sm"
                onClick={() => handleDeleteItem(index)}
                className={styles["delete-button"]}
              >
                <Trash2Icon className={styles.icon} />
              </VariantButton>
            </div>
          ))}
          <VariantButton
            variant="outline"
            size="sm"
            onClick={handleAddItem}
            className={styles["add-item-button"]}
          >
            リスト項目を追加
          </VariantButton>
        </div>
      )}

      <VariantButton
        variant="outline"
        size="sm"
        onClick={handleToggleList}
        className={styles["toggle-list-button"]}
      >
        {props.section.list ? "リストを削除" : "リストを追加"}
      </VariantButton>
    </div>
  );
};
