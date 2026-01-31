"use client";

import { useState } from "react";
import { AvatarUpload } from "@shared/components/molecules/form/avatar-upload";
import { ProfileBasicInfoForm, Value as Basic } from "./basic";
import { ProfileTechStackForm } from "./tech-stack";
import { ProfileCareerForm } from "./career";
import styles from "./index.module.css";
import {
  Profile,
  UnvalidatedCareer,
  UnvalidatedProfile,
} from "@shared/domains/user";
import { UnvalidatedTechnologyStack } from "@shared/domains/common/tech";
import { useServerAction } from "@shared/components/global/hooks/use-server-action";
import { ErrorModal } from "@shared/components/molecules/modal/error";
import { Tag } from "@shared/domains/attributes/tag";

export type Props = {
  initial?: Profile;
  persist: (unvalidated: UnvalidatedProfile) => Promise<void>;
  tags: Tag[];
};

export const ProfileEditForm = (props: Props) => {
  const [avatar, setAvatar] = useState<string>(props.initial?.avatar || "");
  const [basic, setBasic] = useState<Basic>({
    name: props.initial?.name || "",
    bio: props.initial?.bio || "",
    externalServices:
      props.initial?.externalServices.map((service) => ({
        service: service.type,
        user: service.user,
      })) || [],
  });
  const [techStacks, setTechStacks] = useState<UnvalidatedTechnologyStack[]>(
    props.initial?.techStacks
      ? Array.from(props.initial.techStacks.entries())
          .flatMap(([_, stacks]) => stacks)
          .map((techStack) => ({
            tag: techStack.tag,
            from: techStack.from,
            continue: techStack.continue,
            type: techStack.type,
          }))
      : [],
  );
  const [careers, setCareers] = useState<UnvalidatedCareer[]>(
    props.initial?.careers || [],
  );

  const handleAvatarChange = (file: File) => {
    const reader = new FileReader();
    reader.onloadend = () => {
      setAvatar(reader.result as string);
    };
    reader.readAsDataURL(file);
  };

  const handleAddTechStack = () => {
    const newTech: UnvalidatedTechnologyStack = {
      tag: "",
      from: new Date(),
      continue: false,
      type: "personal",
    };
    setTechStacks(techStacks.concat(newTech));
  };

  const handleRemoveTechStack = (index: number) => {
    setTechStacks((previous) => previous.filter((_, i) => i !== index));
  };

  const handleAddCareer = () => {
    const newCareer: UnvalidatedCareer = {
      company: "",
      role: "",
      period: { from: new Date(), to: null },
      description: "",
    };

    setCareers([newCareer].concat(careers));
  };

  const handleRemoveCareer = (index: number) => {
    setCareers((previous) => previous.filter((_, i) => i !== index));
  };

  const handleReorderCareer = (fromIndex: number, toIndex: number) => {
    setCareers((previous) => {
      const newCareers = [...previous];
      const [draggedItem] = newCareers.splice(fromIndex, 1);
      newCareers.splice(toIndex, 0, draggedItem);
      return newCareers;
    });
  };

  const { execute, reset, error, isLoading } = useServerAction(
    async (_: FormData) =>
      props.persist({
        ...basic,
        email: props.initial?.email || "",
        avatar: avatar,
        careers,
        techStacks: new Map(),
        externalServices: basic.externalServices.map((service) => ({
          type: service.service,
          user: service.user,
        })),
      }),
  );

  return (
    <form action={execute} className={styles.container}>
      <div className={styles.avatar}>
        <AvatarUpload
          avatarUrl={avatar}
          displayName={basic.name}
          onAvatarChange={handleAvatarChange}
        />
      </div>

      <ProfileBasicInfoForm value={basic} onUpdate={setBasic} />

      <ProfileTechStackForm
        values={techStacks}
        tags={props.tags}
        onAdd={handleAddTechStack}
        onUpdate={(index, value) => {
          const target = techStacks[index];
          setTechStacks((previous) =>
            previous.map((techStack, i) =>
              i === index ? { ...target, ...value } : techStack,
            ),
          );
        }}
        onRemove={handleRemoveTechStack}
      />

      <ProfileCareerForm
        careers={careers}
        onAdd={handleAddCareer}
        onUpdate={(index, value) => {
          const target = careers[index];
          setCareers((previous) =>
            previous.map((career, i) =>
              i === index ? { ...target, ...value } : career,
            ),
          );
        }}
        onRemove={handleRemoveCareer}
        onReorder={handleReorderCareer}
      />

      <div className={styles.actions}>
        <button type="submit" disabled={isLoading} className={styles.submit}>
          {isLoading ? "更新中..." : "更新する"}
        </button>
      </div>
      <ErrorModal
        isOpen={!!error}
        onClose={reset}
        title="プロフィールの更新に失敗しました"
        message={error?.message ?? "不明なエラーが発生しました"}
        details={error?.details}
      />
    </form>
  );
};
