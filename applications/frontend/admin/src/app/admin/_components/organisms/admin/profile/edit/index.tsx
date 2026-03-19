"use client";

import { AvatarUpload } from "@shared/components/molecules/form/avatar-upload";
import { ProfileBasicInfoForm } from "./basic";
import { ProfileTechStackForm } from "./tech-stack";
import { ProfileCareerForm } from "./career";
import styles from "./index.module.css";
import { Profile, UnvalidatedProfile } from "@shared/domains/user";
import { ErrorModal } from "@shared/components/molecules/modal/error";
import { LoadingOverlay } from "@shared/components/molecules/overlay/loading";
import { useToast } from "@shared/components/molecules/toast";
import { Tag } from "@shared/domains/attributes/tag";
import { useHooks } from "./index.hooks";

export type Props = {
  initial?: Profile;
  persist: (unvalidated: UnvalidatedProfile) => Promise<void>;
  tags: Tag[];
};

export const ProfileEditForm = (props: Props) => {
  const { showToast } = useToast();
  const hooks = useHooks(props.persist, props.initial, () =>
    showToast("プロフィールを更新しました"),
  );

  return (
    <form action={hooks.persist.execute} className={styles.container}>
      <div className={styles.avatar}>
        <AvatarUpload
          avatarUrl={hooks.avatar}
          displayName={hooks.basic.name}
          onAvatarChange={hooks.handleAvatarChange}
        />
      </div>

      <ProfileBasicInfoForm value={hooks.basic} onUpdate={hooks.setBasic} />

      <ProfileTechStackForm
        values={hooks.techStacks}
        tags={props.tags}
        onAdd={hooks.handleAddTechStack}
        onUpdate={hooks.handleUpdateTechStack}
        onRemove={hooks.handleRemoveTechStack}
      />

      <ProfileCareerForm
        careers={hooks.careers}
        onAdd={hooks.handleAddCareer}
        onUpdate={hooks.handleUpdateCareer}
        onRemove={hooks.handleRemoveCareer}
        onReorder={hooks.handleReorderCareer}
      />

      <div className={styles.actions}>
        <button
          type="submit"
          disabled={hooks.persist.isLoading}
          className={styles.submit}
        >
          {hooks.persist.isLoading ? "更新中..." : "更新する"}
        </button>
      </div>
      <ErrorModal
        isOpen={!!hooks.persist.error}
        onClose={hooks.persist.reset}
        title="プロフィールの更新に失敗しました"
        message={
          hooks.persist.error?.message ?? "不明なエラーが発生しました"
        }
        details={hooks.persist.error?.details}
      />
      {hooks.persist.isLoading && <LoadingOverlay />}
    </form>
  );
};
