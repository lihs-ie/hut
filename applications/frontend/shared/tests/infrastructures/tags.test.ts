import { describe, it, expect, beforeEach } from "vitest";
import { FirebaseTagRepository } from "@shared/infrastructures/tags";
import {
  createTestFirestoreWithSeed,
  clearFirestore,
} from "../support/mock/firebase/firestore";
import { Forger } from "@lihs-ie/forger-ts";
import {
  TagMold,
  TagIdentifierMold,
  TagNameMold,
} from "../support/molds/domains/attributes/tag";
import {
  isAggregateNotFoundError,
  isDuplicationError,
} from "@shared/aspects/error";
import {
  validateCriteria,
  type Criteria,
  type TagName,
} from "@shared/domains/attributes/tag";
import { extractError } from "../support/helpers";

describe("infrastructures/tags", () => {
  let testEnvironment: Awaited<ReturnType<typeof createTestFirestoreWithSeed>>;

  beforeEach(async () => {
    testEnvironment = await createTestFirestoreWithSeed({});
  });

  const createRepository = () => {
    type AnyFirestoreOperations = Parameters<typeof FirebaseTagRepository>[1];
    return FirebaseTagRepository(
      testEnvironment.firestore as Parameters<typeof FirebaseTagRepository>[0],
      testEnvironment.operations as AnyFirestoreOperations
    );
  };

  const createCriteria = (parameters: { name?: TagName | null }): Criteria => {
    return validateCriteria({
      name: parameters.name ?? null,
    }).unwrap();
  };

  describe("FirebaseTagRepository", () => {
    describe("persist", () => {
      it("新しいタグを保存できる", async () => {
        const repository = createRepository();
        const tag = Forger(TagMold).forgeWithSeed(1);

        const result = await repository.persist(tag).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(tag.identifier).unwrap();
        expect(found.identifier).toBe(tag.identifier);
        expect(found.name).toBe(tag.name);
      });

      it("既存のタグを更新できる", async () => {
        const repository = createRepository();
        const tag = Forger(TagMold).forgeWithSeed(2);

        await repository.persist(tag).unwrap();

        const newName = Forger(TagNameMold).forgeWithSeed(100, {
          value: "UpdatedTag",
        });
        const updatedTag = Forger(TagMold).forgeWithSeed(0, {
          ...tag,
          name: newName,
        });

        const result = await repository.persist(updatedTag).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(tag.identifier).unwrap();
        expect(found.name).toBe(newName);
      });

      it("存在しないタグを更新しようとするとエラーになる", async () => {
        const repository = createRepository();
        const tag = Forger(TagMold).forgeWithSeed(3);

        await repository.persist(tag).unwrap();

        clearFirestore(testEnvironment.firestore);

        const newName = Forger(TagNameMold).forgeWithSeed(101, {
          value: "UpdatedTag2",
        });
        const updatedTag = Forger(TagMold).forgeWithSeed(0, {
          ...tag,
          name: newName,
        });

        const error = await extractError(repository.persist(updatedTag));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });

      it("重複する名前でタグを作成しようとするとエラーになる", async () => {
        const repository = createRepository();
        const name = Forger(TagNameMold).forgeWithSeed(4, {
          value: "DuplicateName",
        });
        const firstTag = Forger(TagMold).forgeWithSeed(5, { name });

        await repository.persist(firstTag).unwrap();

        const secondTag = Forger(TagMold).forgeWithSeed(6, { name });

        const error = await extractError(repository.persist(secondTag));

        expect(error).not.toBeNull();
        expect(isDuplicationError(error)).toBe(true);
      });
    });

    describe("find", () => {
      it("存在するタグを取得できる", async () => {
        const repository = createRepository();
        const tag = Forger(TagMold).forgeWithSeed(10);

        await repository.persist(tag).unwrap();

        const found = await repository.find(tag.identifier).unwrap();

        expect(found.identifier).toBe(tag.identifier);
        expect(found.name).toBe(tag.name);
        expect(found.logo).toBe(tag.logo);
      });

      it("存在しないタグを取得しようとするとエラーになる", async () => {
        const repository = createRepository();
        const identifier = Forger(TagIdentifierMold).forgeWithSeed(11);

        const error = await extractError(repository.find(identifier));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });
    });

    describe("search", () => {
      it("全てのタグを取得できる", async () => {
        const repository = createRepository();
        const tags = Forger(TagMold).forgeMultiWithSeed(5, 20);

        for (const tag of tags) {
          await repository.persist(tag).unwrap();
        }

        const criteria = createCriteria({});
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(5);
      });

      it("nameで部分検索できる", async () => {
        const repository = createRepository();
        const typeScriptName = Forger(TagNameMold).forgeWithSeed(30, {
          value: "TypeScript",
        });
        const pythonName = Forger(TagNameMold).forgeWithSeed(31, {
          value: "Python",
        });
        const typeScriptTag = Forger(TagMold).forgeWithSeed(32, {
          name: typeScriptName,
        });
        const pythonTag = Forger(TagMold).forgeWithSeed(33, { name: pythonName });

        await repository.persist(typeScriptTag).unwrap();
        await repository.persist(pythonTag).unwrap();

        const criteria = createCriteria({ name: typeScriptName });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.name).toBe(typeScriptName);
      });
    });

    describe("ofIdentifiers", () => {
      it("複数のタグを取得できる", async () => {
        const repository = createRepository();
        const tags = Forger(TagMold).forgeMultiWithSeed(3, 40);

        for (const tag of tags) {
          await repository.persist(tag).unwrap();
        }

        const identifiers = tags.map((tag) => tag.identifier);
        const found = await repository.ofIdentifiers(identifiers).unwrap();

        expect(found.length).toBe(3);
      });

      it("空の配列を渡すと空の配列を返す", async () => {
        const repository = createRepository();

        const found = await repository.ofIdentifiers([]).unwrap();

        expect(found.length).toBe(0);
      });

      it("存在しないタグが含まれてもスキップされる", async () => {
        const repository = createRepository();
        const tag = Forger(TagMold).forgeWithSeed(50);

        await repository.persist(tag).unwrap();

        const nonExistentIdentifier = Forger(TagIdentifierMold).forgeWithSeed(51);
        const identifiers = [tag.identifier, nonExistentIdentifier];

        const found = await repository.ofIdentifiers(identifiers).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.identifier).toBe(tag.identifier);
      });
    });

    describe("ofNames", () => {
      it("名前でタグを取得できる", async () => {
        const repository = createRepository();
        const reactName = Forger(TagNameMold).forgeWithSeed(60, { value: "React" });
        const vueName = Forger(TagNameMold).forgeWithSeed(61, { value: "Vue" });
        const reactTag = Forger(TagMold).forgeWithSeed(62, { name: reactName });
        const vueTag = Forger(TagMold).forgeWithSeed(63, { name: vueName });

        await repository.persist(reactTag).unwrap();
        await repository.persist(vueTag).unwrap();

        const found = await repository.ofNames([reactName, vueName]).unwrap();

        expect(found.length).toBe(2);
      });

      it("存在しない名前はスキップされる", async () => {
        const repository = createRepository();
        const angularName = Forger(TagNameMold).forgeWithSeed(70, {
          value: "Angular",
        });
        const svelteName = Forger(TagNameMold).forgeWithSeed(71, {
          value: "Svelte",
        });
        const angularTag = Forger(TagMold).forgeWithSeed(72, { name: angularName });

        await repository.persist(angularTag).unwrap();

        const found = await repository
          .ofNames([angularName, svelteName])
          .unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.name).toBe(angularName);
      });

      it("空の配列を渡すと空の配列を返す", async () => {
        const repository = createRepository();

        const found = await repository.ofNames([]).unwrap();

        expect(found.length).toBe(0);
      });
    });

    describe("name uniqueness", () => {
      it("同じ名前で新しいタグを作成しようとするとエラーになる", async () => {
        const repository = createRepository();
        const name = Forger(TagNameMold).forgeWithSeed(80, {
          value: "DuplicateName",
        });
        const firstTag = Forger(TagMold).forgeWithSeed(81, { name });
        const secondTag = Forger(TagMold).forgeWithSeed(82, { name });

        await repository.persist(firstTag).unwrap();

        const error = await extractError(repository.persist(secondTag));

        expect(error).not.toBeNull();
        expect(isDuplicationError(error)).toBe(true);
      });

      it("タグ更新時に他のタグと同じ名前に変更しようとするとエラーになる", async () => {
        const repository = createRepository();
        const firstName = Forger(TagNameMold).forgeWithSeed(90, {
          value: "NameOne",
        });
        const secondName = Forger(TagNameMold).forgeWithSeed(91, {
          value: "NameTwo",
        });

        const firstTag = Forger(TagMold).forgeWithSeed(92, { name: firstName });
        const secondTag = Forger(TagMold).forgeWithSeed(93, { name: secondName });

        await repository.persist(firstTag).unwrap();
        await repository.persist(secondTag).unwrap();

        const updatedSecondTag = Forger(TagMold).forgeWithSeed(0, {
          ...secondTag,
          name: firstName,
        });

        const error = await extractError(repository.persist(updatedSecondTag));

        expect(error).not.toBeNull();
        expect(isDuplicationError(error)).toBe(true);
      });

      it("タグ削除後に同じ名前で新しいタグを作成できる", async () => {
        const repository = createRepository();
        const name = Forger(TagNameMold).forgeWithSeed(100, {
          value: "ReusableName",
        });
        const firstTag = Forger(TagMold).forgeWithSeed(101, { name });

        await repository.persist(firstTag).unwrap();
        await repository.terminate(firstTag.identifier).unwrap();

        const secondTag = Forger(TagMold).forgeWithSeed(102, { name });
        const result = await repository.persist(secondTag).unwrap();

        expect(result).toBeUndefined();
      });

      it("同じタグの名前を変更できる", async () => {
        const repository = createRepository();
        const oldName = Forger(TagNameMold).forgeWithSeed(110, {
          value: "OldName",
        });
        const newName = Forger(TagNameMold).forgeWithSeed(111, {
          value: "NewName",
        });

        const tag = Forger(TagMold).forgeWithSeed(112, { name: oldName });

        await repository.persist(tag).unwrap();

        const updatedTag = Forger(TagMold).forgeWithSeed(0, {
          ...tag,
          name: newName,
        });

        const result = await repository.persist(updatedTag).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(tag.identifier).unwrap();
        expect(found.name).toBe(newName);
      });
    });

    describe("terminate", () => {
      it("タグを削除できる", async () => {
        const repository = createRepository();
        const tag = Forger(TagMold).forgeWithSeed(120);

        await repository.persist(tag).unwrap();

        const result = await repository.terminate(tag.identifier).unwrap();

        expect(result).toBeUndefined();

        const error = await extractError(repository.find(tag.identifier));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });

      it("存在しないタグを削除しようとするとエラーになる", async () => {
        const repository = createRepository();
        const identifier = Forger(TagIdentifierMold).forgeWithSeed(130);

        const error = await extractError(repository.terminate(identifier));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });
    });
  });
});
