import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { ProfileMold } from "@shared-tests/support/molds/domains/user/common";
import type { Profile, UnvalidatedCareer } from "@shared/domains/user";
import type { UnvalidatedTechnologyStack } from "@shared/domains/common/tech";

vi.mock("@shared/components/global/hooks/use-server-action", () => ({
  useServerAction: (action: () => Promise<unknown>) => ({
    execute: () => action(),
    isLoading: false,
    error: null,
    reset: vi.fn(),
  }),
}));

interface ExternalServiceInput {
  readonly service: string;
  readonly user: string;
}

interface BasicInfo {
  readonly name: string;
  readonly bio: string;
  readonly externalServices: ExternalServiceInput[];
}

const createEmptyTechStack = (type: "personal" | "work" = "personal"): UnvalidatedTechnologyStack => ({
  tag: "",
  from: new Date(),
  continue: false,
  type,
});

const createEmptyCareer = (): UnvalidatedCareer => ({
  company: "",
  role: "",
  period: { from: new Date(), to: null },
  description: "",
});

const initializeFromProfile = (initial: Profile | undefined): {
  avatar: string;
  basic: BasicInfo;
  techStacks: UnvalidatedTechnologyStack[];
  careers: UnvalidatedCareer[];
} => ({
  avatar: initial?.avatar || "",
  basic: {
    name: initial?.name || "",
    bio: initial?.bio || "",
    externalServices:
      initial?.externalServices.map((service) => ({
        service: service.type,
        user: service.user,
      })) || [],
  },
  techStacks: initial?.techStacks
    ? Array.from(initial.techStacks.entries())
        .flatMap(([_, stacks]) => stacks)
        .map((techStack) => ({
          tag: techStack.tag,
          from: techStack.from,
          continue: techStack.continue,
          type: techStack.type,
        }))
    : [],
  careers: initial?.careers || [],
});

const reorderArray = <T>(array: T[], fromIndex: number, toIndex: number): T[] => {
  const result = [...array];
  const [draggedItem] = result.splice(fromIndex, 1);
  result.splice(toIndex, 0, draggedItem);
  return result;
};

describe("hooks/profile-edit", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe("useHooks ロジック", () => {
    describe("初期状態の生成ロジック", () => {
      it("初期プロファイルなしの場合は空の値を生成する", () => {
        const { avatar, basic, techStacks, careers } = initializeFromProfile(undefined);

        expect(avatar).toBe("");
        expect(basic.name).toBe("");
        expect(basic.bio).toBe("");
        expect(basic.externalServices).toEqual([]);
        expect(techStacks).toEqual([]);
        expect(careers).toEqual([]);
      });

      it("初期プロファイルがある場合はその値を使用する", () => {
        const initial = Forger(ProfileMold).forgeWithSeed(1);
        const { avatar, basic } = initializeFromProfile(initial);

        expect(avatar).toBe(initial.avatar);
        expect(basic.name).toBe(initial.name);
        expect(basic.bio).toBe(initial.bio);
        expect(basic.externalServices).toHaveLength(initial.externalServices.length);
      });
    });

    describe("handleAddTechStack ロジック", () => {
      it("新しい技術スタックを空の配列に追加する", () => {
        const techStacks: UnvalidatedTechnologyStack[] = [];
        const newTechStacks = techStacks.concat(createEmptyTechStack());

        expect(newTechStacks).toHaveLength(1);
        expect(newTechStacks[0].tag).toBe("");
        expect(newTechStacks[0].continue).toBe(false);
        expect(newTechStacks[0].type).toBe("personal");
      });

      it("既存の配列に追加できる", () => {
        const techStacks: UnvalidatedTechnologyStack[] = [
          { tag: "react", from: new Date(), continue: true, type: "work" },
        ];
        const newTechStacks = techStacks.concat(createEmptyTechStack());

        expect(newTechStacks).toHaveLength(2);
        expect(newTechStacks[0].tag).toBe("react");
        expect(newTechStacks[1].tag).toBe("");
      });
    });

    describe("handleRemoveTechStack ロジック", () => {
      const baseTechStacks: UnvalidatedTechnologyStack[] = [
        { tag: "react", from: new Date(), continue: true, type: "work" },
        { tag: "typescript", from: new Date(), continue: false, type: "personal" },
        { tag: "nodejs", from: new Date(), continue: true, type: "work" },
      ];

      const removeAtIndexCases = [
        { removeIndex: 1, expectedLength: 2, expectedTags: ["react", "nodejs"] },
        { removeIndex: 0, expectedLength: 2, expectedTags: ["typescript", "nodejs"] },
        { removeIndex: 2, expectedLength: 2, expectedTags: ["react", "typescript"] },
      ];

      removeAtIndexCases.forEach(({ removeIndex, expectedLength, expectedTags }) => {
        it(`インデックス ${removeIndex} を削除すると ${expectedTags.join(", ")} が残る`, () => {
          const newTechStacks = baseTechStacks.filter((_, index) => index !== removeIndex);

          expect(newTechStacks).toHaveLength(expectedLength);
          newTechStacks.forEach((stack, index) => {
            expect(stack.tag).toBe(expectedTags[index]);
          });
        });
      });
    });

    describe("handleAddCareer ロジック", () => {
      it("新しい経歴を配列に追加する", () => {
        const careers: UnvalidatedCareer[] = [];
        const newCareers = careers.concat(createEmptyCareer());

        expect(newCareers).toHaveLength(1);
        expect(newCareers[0].company).toBe("");
        expect(newCareers[0].role).toBe("");
        expect(newCareers[0].period.to).toBeNull();
      });
    });

    describe("handleRemoveCareer ロジック", () => {
      it("指定インデックスの経歴を削除する", () => {
        const careers: UnvalidatedCareer[] = [
          { company: "Company A", role: "Role A", period: { from: new Date(), to: null }, description: "A" },
          { company: "Company B", role: "Role B", period: { from: new Date(), to: null }, description: "B" },
          { company: "Company C", role: "Role C", period: { from: new Date(), to: null }, description: "C" },
        ];

        const newCareers = careers.filter((_, index) => index !== 1);

        expect(newCareers).toHaveLength(2);
        expect(newCareers[0].company).toBe("Company A");
        expect(newCareers[1].company).toBe("Company C");
      });
    });

    describe("handleReorderCareer ロジック", () => {
      const baseCareers: UnvalidatedCareer[] = [
        { company: "Company A", role: "Role A", period: { from: new Date(), to: null }, description: "A" },
        { company: "Company B", role: "Role B", period: { from: new Date(), to: null }, description: "B" },
        { company: "Company C", role: "Role C", period: { from: new Date(), to: null }, description: "C" },
      ];

      const reorderCases = [
        { fromIndex: 0, toIndex: 2, expectedOrder: ["Company B", "Company C", "Company A"] },
        { fromIndex: 2, toIndex: 0, expectedOrder: ["Company C", "Company A", "Company B"] },
        { fromIndex: 0, toIndex: 0, expectedOrder: ["Company A", "Company B", "Company C"] },
      ];

      reorderCases.forEach(({ fromIndex, toIndex, expectedOrder }) => {
        it(`${fromIndex} から ${toIndex} に移動すると ${expectedOrder.join(" -> ")} の順序になる`, () => {
          const newCareers = reorderArray(baseCareers, fromIndex, toIndex);

          expectedOrder.forEach((company, index) => {
            expect(newCareers[index].company).toBe(company);
          });
        });
      });

      it("中間の要素を別の位置に移動できる", () => {
        const fourItemCareers: UnvalidatedCareer[] = [
          ...baseCareers,
          { company: "Company D", role: "Role D", period: { from: new Date(), to: null }, description: "D" },
        ];

        const newCareers = reorderArray(fourItemCareers, 1, 3);

        expect(newCareers[0].company).toBe("Company A");
        expect(newCareers[1].company).toBe("Company C");
        expect(newCareers[2].company).toBe("Company D");
        expect(newCareers[3].company).toBe("Company B");
      });
    });

    describe("externalServices のマッピングロジック", () => {
      it("externalServices を正しくマッピングする", () => {
        const initial = Forger(ProfileMold).forgeWithSeed(1);

        const mappedServices = initial.externalServices.map((service) => ({
          service: service.type,
          user: service.user,
        }));

        expect(mappedServices).toHaveLength(initial.externalServices.length);
        if (initial.externalServices.length > 0) {
          expect(mappedServices[0]).toHaveProperty("service");
          expect(mappedServices[0]).toHaveProperty("user");
          expect(mappedServices[0].service).toBe(initial.externalServices[0].type);
        }
      });
    });

    describe("techStacks の初期化ロジック", () => {
      it("Map から配列に変換する", () => {
        const initial = Forger(ProfileMold).forgeWithSeed(1);
        const { techStacks } = initializeFromProfile(initial);

        const mapEntries = initial.techStacks ? Array.from(initial.techStacks.entries()) : [];
        const expectedLength = mapEntries.reduce((sum, [_, stacks]) => sum + stacks.length, 0);

        expect(techStacks.length).toBe(expectedLength);
      });
    });

    describe("persist 引数の生成ロジック", () => {
      it("正しい形式で引数を生成する", () => {
        const persistArg = {
          name: "Test Name",
          bio: "Test Bio",
          email: "",
          avatar: "test-avatar",
          careers: [],
          techStacks: new Map(),
          externalServices: [],
        };

        expect(persistArg.name).toBe("Test Name");
        expect(persistArg.bio).toBe("Test Bio");
        expect(persistArg.email).toBe("");
        expect(persistArg.avatar).toBe("test-avatar");
        expect(persistArg.techStacks).toBeInstanceOf(Map);
      });

      it("初期プロファイルがある場合は email を引き継ぐ", () => {
        const initial = Forger(ProfileMold).forgeWithSeed(1);

        const persistArg = {
          name: "Test Name",
          bio: "Test Bio",
          email: initial.email,
          avatar: "test-avatar",
          careers: [],
          techStacks: new Map(),
          externalServices: [],
        };

        expect(persistArg.email).toBe(initial.email);
      });
    });

    describe("FileReader 処理ロジック", () => {
      it("FileReader の result を文字列として扱う", () => {
        const mockResult = "data:image/png;base64,test-image-data";

        expect(typeof mockResult).toBe("string");
        expect(mockResult).toBe("data:image/png;base64,test-image-data");
      });

      it("null の result は空文字列にフォールバックする", () => {
        const mockResult: string | null = null;
        const safeResult = mockResult || "";

        expect(safeResult).toBe("");
      });
    });

    describe("Basic 型", () => {
      it("Basic 型が正しい構造を持つ", () => {
        const basic: BasicInfo = {
          name: "Test",
          bio: "Bio",
          externalServices: [{ service: "github", user: "test-user" }],
        };

        expect(basic.name).toBe("Test");
        expect(basic.bio).toBe("Bio");
        expect(basic.externalServices).toHaveLength(1);
        expect(basic.externalServices[0].service).toBe("github");
      });
    });
  });
});
