"use client";

import { useState } from "react";
import {
  PolicySectionEditor,
  EditablePolicySection,
} from "@shared/components/molecules/editor/policy-section";
import { VariantButton } from "@shared/components/atoms/button/variant";
import { PlusIcon } from "@shared/components/atoms/icon";
import styles from "./editor.module.css";
import { UnvalidatedPrivacyPolicy } from "@shared/domains/document";
import { useServerAction } from "@shared/hooks";
import { ErrorModal } from "@shared/components/molecules/modal/error";

export type Props = {
  value: UnvalidatedPrivacyPolicy;
  persist: (unvalidated: UnvalidatedPrivacyPolicy) => Promise<void>;
};

export const PolicyEditorPresenter = (props: Props) => {
  const { execute, reset, error, isLoading } = useServerAction(props.persist);

  const [sections, setSections] = useState<
    UnvalidatedPrivacyPolicy["sections"]
  >(props.value.sections);

  const handleUpdateSection = (
    index: number,
    updatedSection: EditablePolicySection,
  ) => {
    const newSections = [...sections];
    newSections[index] = updatedSection;
    setSections(newSections);
  };

  const handleDeleteSection = (index: number) => {
    setSections(sections.filter((_, i) => i !== index));
  };

  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <h1 className={styles.title}>プライバシーポリシー</h1>
        <VariantButton
          onClick={() =>
            execute({
              sections,
              timeline: {
                createdAt: props.value.timeline.createdAt,
                updatedAt: new Date(),
              },
            })
          }
          disabled={isLoading}
          className={styles["save-button"]}
        >
          {isLoading ? "保存中..." : "保存する"}
        </VariantButton>
      </div>

      <div className={styles.sections}>
        {sections.map((section, index) => (
          <PolicySectionEditor
            key={index}
            section={section}
            onUpdate={(updated) => handleUpdateSection(index, updated)}
            onDelete={() => handleDeleteSection(index)}
            showDelete={sections.length > 1}
          />
        ))}
      </div>

      <VariantButton
        variant="outline"
        onClick={() => {
          setSections(
            sections.concat([{ headline: "", body: "", list: null }]),
          );
        }}
        className={styles["add-button"]}
      >
        <PlusIcon className={styles.icon} />
        セクションを追加
      </VariantButton>
      <ErrorModal
        isOpen={!!error}
        onClose={reset}
        title="保存に失敗しました"
        message={error?.message ?? ""}
        details={error?.details}
      />
    </div>
  );
};
