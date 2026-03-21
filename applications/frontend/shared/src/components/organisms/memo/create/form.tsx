"use client";

import { Textarea } from "@shared/components/atoms/input/textarea";
import { TextInput } from "@shared/components/atoms/input/text";
import styles from "./form.module.css";
import { PublishStatus, publishStatusSchema, slugSchema } from "@shared/domains/common";
import { useState } from "react";
import { UnvalidatedMemo } from "@shared/domains/memo";
import { FormButton } from "@shared/components/atoms/button/form";
import { DropdownSelect } from "@shared/components/atoms/select/dropdown";
import { useServerAction } from "@shared/components/global/hooks/use-server-action";
import { ErrorModal } from "@shared/components/molecules/modal/error";
import { LoadingOverlay } from "@shared/components/molecules/overlay/loading";
import { useToast } from "@shared/components/molecules/toast";
import { ulid } from "ulid";
import { useRouter } from "next/navigation";
import { Routes } from "@shared/config/presentation/route";

export type Props = {
  persist: (unvalidated: UnvalidatedMemo) => Promise<void>;
};

export const MemoCreateForm = (props: Props) => {
  const router = useRouter();
  const { showToast } = useToast();
  const [title, setTitle] = useState("");
  const [slug, setSlug] = useState("");
  const [status, setStatus] = useState<PublishStatus>(PublishStatus.DRAFT);

  const { execute, reset, error, isLoading } = useServerAction(
    async () => {
      const unvalidated: UnvalidatedMemo = {
        identifier: ulid(),
        title,
        slug,
        tags: [],
        entries: [],
        images: [],
        status,
        timeline: {
          createdAt: new Date(),
          updatedAt: new Date(),
        },
      };

      await props.persist(unvalidated);

      router.push(Routes.page.memos.edit(slug));
    },
    { onSuccess: () => showToast("メモを作成しました") },
  );

  const isSlugValid = slugSchema.safeParse(slug).success;

  return (
    <form className={styles.container} action={execute}>
      <div className={styles.input}>
        <Textarea
          min={1}
          max={100}
          value={title}
          onChange={setTitle}
          placeholder="Enter title..."
          name="title"
        />
      </div>
      <div className={styles.input}>
        <TextInput
          value={slug}
          onChange={setSlug}
          placeholder="Enter slug..."
        />
      </div>
      <div className={styles.button}>
        <FormButton
          disabled={
            title.length === 0 ||
            100 < title.length ||
            !isSlugValid ||
            isLoading
          }
        >
          {isLoading ? "作成中..." : "メモを作成"}
        </FormButton>
      </div>
      <div className={styles.select}>
        <DropdownSelect
          value={status}
          onChange={(value) => setStatus(publishStatusSchema.parse(value))}
          options={[PublishStatus.DRAFT, PublishStatus.PUBLISHED].map(
            (status) => ({
              value: status,
              label: status === PublishStatus.DRAFT ? "下書き" : "公開",
            }),
          )}
          name="status"
        />
      </div>
      <ErrorModal
        isOpen={!!error}
        onClose={reset}
        title="メモの作成に失敗しました"
        message={error?.message ?? ""}
        details={error?.details}
      />
      {isLoading && <LoadingOverlay />}
    </form>
  );
};
