"use client";

import styles from "./content.module.css";
import { AdminContentCard, Props as Content } from "./card/content";
import { useServerAction } from "@shared/hooks";
import { LoadingOverlay } from "@shared/components/molecules/overlay/loading";
import { ConfirmModal } from "@shared/components/molecules/modal/confirm";
import { useState } from "react";
import { ErrorModal } from "@shared/components/molecules/modal/error";

export type Props = {
  contents: Content[];
};

export const AdminContentListPresenter = (props: Props) => {
  const [pendingDeleteContent, setPendingDeleteContent] =
    useState<Content | null>(null);

  const {
    execute: executeTerminate,
    error,
    reset,
    isLoading,
  } = useServerAction(async (content: Content) => {
    await content.onTerminate();
  });

  const handleTerminate = (content: Content) => {
    setPendingDeleteContent(content);
  };

  const handleConfirm = async () => {
    if (pendingDeleteContent) {
      await executeTerminate(pendingDeleteContent);
      setPendingDeleteContent(null);
    }
  };

  const handleClose = () => {
    setPendingDeleteContent(null);
    reset();
  };

  return (
    <div className={styles.container}>
      {props.contents.map((content) => (
        <AdminContentCard
          key={content.title}
          {...content}
          onTerminate={() => handleTerminate(content)}
        />
      ))}
      <ConfirmModal
        isOpen={pendingDeleteContent !== null}
        onClose={handleClose}
        onConfirm={handleConfirm}
        title="コンテンツの削除"
        message={
          pendingDeleteContent
            ? `「${pendingDeleteContent.title}」を削除しますか？この操作は元に戻せません。`
            : ""
        }
        confirmText="削除する"
        cancelText="キャンセル"
        type="warning"
      />
      <ErrorModal
        isOpen={!!error}
        onClose={reset}
        message={error?.message ?? ""}
        details={error?.details}
      />
      {isLoading && <LoadingOverlay />}
    </div>
  );
};
