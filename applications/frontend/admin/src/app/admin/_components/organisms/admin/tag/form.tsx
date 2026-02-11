"use client";

import { useState } from "react";
import { Trash2Icon } from "@shared/components/atoms/icon/trash";
import { Tag, UnvalidatedTag } from "@shared/domains/attributes/tag";
import { TagForm } from "@shared/components/molecules/form/tag";
import { ConfirmModal } from "@shared/components/molecules/modal/confirm";
import styles from "./form.module.css";
import { useRouter } from "next/navigation";
import { VariantButton } from "@shared/components/atoms/button/variant";
import { useServerAction } from "@shared/components/global/hooks/use-server-action";
import { ulid } from "ulid";
import { ErrorModal } from "@shared/components/molecules/modal/error";

export type Props = {
  initial?: Tag;
  persist: (unvalidated: UnvalidatedTag) => Promise<void>;
  terminate: (identifier: string) => Promise<void>;
};

export const TagEditForm = (props: Props) => {
  const router = useRouter();

  const [name, setName] = useState(props.initial?.name || "");
  const [logoSrc, setLogoSrc] = useState<string>(props.initial?.logo || "");

  const [isOpenConfirmOpen, setIsOpenConfirmOpen] = useState(false);

  const {
    execute: persist,
    reset: resetPersist,
    error: persistError,
    isLoading: isPersisting,
  } = useServerAction(async (_: FormData) => {
    await props.persist({
      identifier: props.initial?.identifier || ulid(),
      name: name.trim(),
      logo: logoSrc,
      timeline: {
        createdAt: props.initial?.timeline.createdAt || new Date(),
        updatedAt: new Date(),
      },
    });

    router.push("/admin/tags");
  });

  const {
    execute: terminate,
    reset: resetTerminate,
    error: terminateError,
    isLoading: isTerminating,
  } = useServerAction(async () => {
    if (!!props.initial) {
      await props.terminate(props.initial.identifier);
      setIsOpenConfirmOpen(false);
      router.push("/admin/tags");
    }
  });

  return (
    <form action={persist} className={styles.container}>
      <TagForm
        name={name}
        onNameChange={setName}
        logoPreview={logoSrc}
        onLogoChange={setLogoSrc}
      />

      <div className={styles.actions}>
        <button
          type="submit"
          disabled={isPersisting || !name.trim() || !logoSrc.trim()}
          className={`${styles.button} ${styles["button-primary"]}`}
        >
          {isPersisting ? "保存中..." : props.initial ? "更新する" : "作成する"}
        </button>

        <VariantButton
          type="button"
          onClick={() => router.push("/admin/tags")}
          disabled={isPersisting || isTerminating}
        >
          キャンセル
        </VariantButton>

        {props.initial && (
          <button
            type="button"
            onClick={() => setIsOpenConfirmOpen(true)}
            className={`${styles.button} ${styles["button-destructive"]}`}
            disabled={isPersisting || isTerminating}
          >
            {isTerminating ? (
              "削除中..."
            ) : (
              <>
                <Trash2Icon className={styles.icon} />
                タグを削除
              </>
            )}
          </button>
        )}
      </div>

      {props.initial && (
        <ConfirmModal
          isOpen={isOpenConfirmOpen}
          onClose={() => setIsOpenConfirmOpen(false)}
          onConfirm={terminate}
          title="タグを削除"
          message={`「${name}」を削除してもよろしいですか？この操作は取り消せません。`}
          type="error"
          confirmText="削除する"
          cancelText="キャンセル"
        />
      )}
      <ErrorModal
        isOpen={!!persistError || !!terminateError}
        onClose={() => {
          resetPersist();
          resetTerminate();
        }}
        message={persistError?.message ?? terminateError?.message ?? ""}
        details={persistError?.details ?? terminateError?.details}
      />
    </form>
  );
};
