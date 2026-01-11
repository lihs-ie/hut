import { UnvalidatedTechnologyStack } from "@shared/domains/common/tech";
import {
  Profile,
  UnvalidatedCareer,
  UnvalidatedProfile,
} from "@shared/domains/user";
import { useServerAction } from "@shared/hooks";
import { Dispatch, SetStateAction, useState } from "react";
import { Value as Basic } from "./basic";
import { ServerActionError } from "@shared/hooks/useServerAction";

type Hooks = {
  avatar: string;
  setAvatar: Dispatch<SetStateAction<string>>;
  basic: Basic;
  setBasic: Dispatch<SetStateAction<Basic>>;
  techStacks: UnvalidatedTechnologyStack[];
  setTechStacks: Dispatch<SetStateAction<UnvalidatedTechnologyStack[]>>;
  careers: UnvalidatedCareer[];
  setCareers: Dispatch<SetStateAction<UnvalidatedCareer[]>>;
  handleAvatarChange: (file: File) => void;
  handleAddTechStack: () => void;
  handleRemoveTechStack: (index: number) => void;
  handleAddCareer: () => void;
  handleRemoveCareer: (index: number) => void;
  handleReorderCareer: (fromIndex: number, toIndex: number) => void;
  persist: {
    execute: () => Promise<void>;
    reset: () => void;
    error: ServerActionError | null;
    isLoading: boolean;
  };
};

export const useHooks = (
  persist: (unvalidated: UnvalidatedProfile) => Promise<void>,
  initial?: Profile,
): Hooks => {
  const [avatar, setAvatar] = useState<string>(initial?.avatar || "");
  const [basic, setBasic] = useState<Basic>({
    name: initial?.name || "",
    bio: initial?.bio || "",
    externalServices:
      initial?.externalServices.map((service) => ({
        service: service.type,
        user: service.user,
      })) || [],
  });
  const [techStacks, setTechStacks] = useState<UnvalidatedTechnologyStack[]>(
    initial?.techStacks
      ? Array.from(initial.techStacks.entries())
          .flatMap(([_, stacks]) => stacks)
          .map((techStack) => ({
            kind: techStack.kind,
            from: techStack.from,
            continue: techStack.continue,
            type: techStack.type,
          }))
      : [],
  );
  const [careers, setCareers] = useState<UnvalidatedCareer[]>(
    initial?.careers || [],
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
      kind: "",
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
    setCareers(careers.concat(newCareer));
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

  const { execute, reset, error, isLoading } = useServerAction(async () =>
    persist({
      ...basic,
      email: initial?.email || "",
      avatar: avatar,
      careers,
      techStacks: new Map(),
      externalServices: [],
    }),
  );

  return {
    avatar,
    setAvatar,
    basic,
    setBasic,
    techStacks,
    setTechStacks,
    careers,
    setCareers,
    handleAvatarChange,
    handleAddTechStack,
    handleRemoveTechStack,
    handleAddCareer,
    handleRemoveCareer,
    handleReorderCareer,
    persist: { execute, reset, error, isLoading },
  };
};
