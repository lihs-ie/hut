"use client";

import { PublishStatus } from "@shared/domains/common";
import styles from "./sidebar.module.css";
import { SimpleCard } from "@shared/components/atoms/card/simple";
import { CheckIcon } from "@shared/components/atoms/icon/check";
import { GlobeIcon } from "@shared/components/atoms/icon/globe";
import { LockIcon } from "@shared/components/atoms/icon/lock";
import { useState } from "react";
import { SimpleSwitch } from "@shared/components/atoms/toggle/simple";
import { ConfirmModal } from "@shared/components/molecules/modal/confirm";
import { useRouter } from "next/navigation";
import { VariantButton } from "@shared/components/atoms/button/variant";
import { useServerAction } from "@shared/hooks";
import { ErrorModal } from "@shared/components/molecules/modal/error";
import { LoadingOverlay } from "@shared/components/molecules/overlay/loading";

export type Props = {
  initialStatus: PublishStatus;
  changeStatus: (next: PublishStatus) => Promise<void>;
};

export const MemoEditSidebarPresenter = (props: Props) => {
  const router = useRouter();
  const [isPublished, setIsPublished] = useState(
    props.initialStatus === PublishStatus.PUBLISHED,
  );
  const [isModalOpen, setIsModalOpen] = useState(false);

  const {
    execute: executeClose,
    error: closeError,
    isLoading: isCloseLoading,
    reset: resetClose,
  } = useServerAction(async (status: PublishStatus) => {
    await props.changeStatus(status);
    router.push("/admin/memos");
  });

  const {
    execute: executeVisibility,
    error: visibilityError,
    isLoading: isVisibilityLoading,
    reset: resetVisibility,
  } = useServerAction(async (status: PublishStatus) => {
    await props.changeStatus(status);
    setIsPublished(status === PublishStatus.PUBLISHED);
  });

  return (
    <aside className={styles.container}>
      <SimpleCard className={styles.card}>
        <VariantButton
          onClick={async () => await executeClose(PublishStatus.ARCHIVED)}
          className={styles.button}
          variant="default"
        >
          <CheckIcon className={styles.icon} />
          メモをクローズ
        </VariantButton>

        <div className={styles.section}>
          <div className={styles.row}>
            {isPublished ? (
              <GlobeIcon className={styles.icon} />
            ) : (
              <LockIcon className={styles.icon} />
            )}
            <span className={styles["status-label"]}>
              {isPublished ? "公開" : "下書き"}
            </span>
          </div>

          <div className={styles.row}>
            <span className={styles["text-muted"]}>
              {isPublished ? "公開" : "下書き"}
            </span>
            <SimpleSwitch
              checked={isPublished}
              onChange={async (checked) =>
                await executeVisibility(
                  checked ? PublishStatus.PUBLISHED : PublishStatus.DRAFT,
                )
              }
            />
          </div>
        </div>
      </SimpleCard>
      {isCloseLoading || (isVisibilityLoading && <LoadingOverlay />)}
      <ErrorModal
        isOpen={!!closeError || !!visibilityError}
        onClose={() => {
          resetClose();
          resetVisibility();
        }}
        message={closeError?.message ?? visibilityError?.message ?? ""}
        details={closeError?.details ?? visibilityError?.details}
      />
      {isModalOpen && (
        <ConfirmModal
          isOpen={isModalOpen}
          onClose={() => setIsModalOpen(false)}
          onConfirm={async () => {
            setIsModalOpen(false);
            await executeClose(PublishStatus.ARCHIVED);
          }}
          title="メモをクローズしますか？"
          message="クローズすると、このメモは完了済みとしてマークされ、詳細ページに移動します。"
          confirmText="クローズする"
          cancelText="キャンセル"
          type="warning"
        />
      )}
    </aside>
  );
};
