import { UnvalidatedTechnologyStack } from "@shared/domains/common/tech";
import {
  Profile,
  UnvalidatedCareer,
  UnvalidatedProfile,
} from "@shared/domains/user";
import {
  useServerAction,
  ServerActionError,
} from "@shared/components/global/hooks/use-server-action";
import { Dispatch, SetStateAction, useState } from "react";
import { Value as Basic } from "./basic";

export type TechStackWithCategory = UnvalidatedTechnologyStack & {
  category: string;
};

type Hooks = {
  avatar: string;
  setAvatar: Dispatch<SetStateAction<string>>;
  basic: Basic;
  setBasic: Dispatch<SetStateAction<Basic>>;
  techStacks: TechStackWithCategory[];
  setTechStacks: Dispatch<SetStateAction<TechStackWithCategory[]>>;
  careers: UnvalidatedCareer[];
  setCareers: Dispatch<SetStateAction<UnvalidatedCareer[]>>;
  handleAvatarChange: (file: File) => void;
  handleAddTechStack: () => void;
  handleRemoveTechStack: (index: number) => void;
  handleUpdateTechStack: (
    index: number,
    value: UnvalidatedTechnologyStack,
  ) => void;
  handleAddCareer: () => void;
  handleRemoveCareer: (index: number) => void;
  handleUpdateCareer: (index: number, value: UnvalidatedCareer) => void;
  handleReorderCareer: (fromIndex: number, toIndex: number) => void;
  persist: {
    execute: (_formData: FormData) => Promise<void>;
    reset: () => void;
    error: ServerActionError | null;
    isLoading: boolean;
  };
};

export const initializeFromProfileWithCategory = (
  initial: Profile | undefined,
): TechStackWithCategory[] => {
  if (!initial?.techStacks) {
    return [];
  }
  return Array.from(initial.techStacks.entries()).flatMap(
    ([category, stacks]) =>
      stacks.map((techStack) => ({
        tag: techStack.tag,
        from: techStack.from,
        continue: techStack.continue,
        type: techStack.type,
        category,
      })),
  );
};

export const buildTechStacksMap = (
  techStacks: TechStackWithCategory[],
): Map<
  string,
  { tag: string; from: Date; continue: boolean; type: string }[]
> => {
  const result = new Map<
    string,
    { tag: string; from: Date; continue: boolean; type: string }[]
  >();
  for (const techStack of techStacks) {
    const existing = result.get(techStack.category) ?? [];
    result.set(techStack.category, [
      ...existing,
      {
        tag: techStack.tag,
        from: techStack.from,
        continue: techStack.continue,
        type: techStack.type,
      },
    ]);
  }
  return result;
};

export const useHooks = (
  persistAction: (unvalidated: UnvalidatedProfile) => Promise<void>,
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
  const [techStacks, setTechStacks] = useState<TechStackWithCategory[]>(
    initializeFromProfileWithCategory(initial),
  );
  const [careers, setCareers] = useState<UnvalidatedCareer[]>(
    initial?.careers || [],
  );

  const handleAvatarChange = (file: File) => {
    const reader = new FileReader();
    reader.onloadend = () => {
      if (typeof reader.result === "string") {
        setAvatar(reader.result);
      }
    };
    reader.readAsDataURL(file);
  };

  const handleAddTechStack = () => {
    const newTechStack: TechStackWithCategory = {
      tag: "",
      from: new Date(),
      continue: false,
      type: "personal",
      category: "backend",
    };
    setTechStacks((previous) => previous.concat(newTechStack));
  };

  const handleRemoveTechStack = (index: number) => {
    setTechStacks((previous) => previous.filter((_, i) => i !== index));
  };

  const handleUpdateTechStack = (
    index: number,
    value: UnvalidatedTechnologyStack,
  ) => {
    setTechStacks((previous) =>
      previous.map((techStack, i) =>
        i === index ? { ...techStack, ...value } : techStack,
      ),
    );
  };

  const handleAddCareer = () => {
    const newCareer: UnvalidatedCareer = {
      company: "",
      role: "",
      period: { from: new Date(), to: null },
      description: "",
    };
    setCareers((previous) => [newCareer].concat(previous));
  };

  const handleRemoveCareer = (index: number) => {
    setCareers((previous) => previous.filter((_, i) => i !== index));
  };

  const handleUpdateCareer = (index: number, value: UnvalidatedCareer) => {
    setCareers((previous) =>
      previous.map((career, i) =>
        i === index ? { ...career, ...value } : career,
      ),
    );
  };

  const handleReorderCareer = (fromIndex: number, toIndex: number) => {
    setCareers((previous) => {
      const reordered = [...previous];
      const [draggedItem] = reordered.splice(fromIndex, 1);
      reordered.splice(toIndex, 0, draggedItem);
      return reordered;
    });
  };

  const { execute, reset, error, isLoading } = useServerAction(
    async (_formData: FormData) =>
      persistAction({
        ...basic,
        email: initial?.email || "",
        avatar: avatar,
        careers,
        techStacks: buildTechStacksMap(techStacks),
        externalServices: basic.externalServices.map((service) => ({
          type: service.service,
          user: service.user,
        })),
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
    handleUpdateTechStack,
    handleAddCareer,
    handleRemoveCareer,
    handleUpdateCareer,
    handleReorderCareer,
    persist: { execute, reset, error, isLoading },
  };
};
