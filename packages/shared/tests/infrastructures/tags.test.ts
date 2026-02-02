import { describe, it, expect, beforeEach } from "vitest";
import { FirebaseTagRepository } from "@shared/infrastructures/tags";
import {
  createTestFirestoreWithSeed,
  clearFirestore,
} from "../support/mock/firebase/firestore";
import { Builder } from "../support/molds";
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
    // Note: Type assertion needed because mock FirestoreOperations has slightly different internal types
    // This is acceptable for testing purposes as the runtime behavior is identical
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
        const tag = Builder(TagMold).buildWith(1);

        const result = await repository.persist(tag).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(tag.identifier).unwrap();
        expect(found.identifier).toBe(tag.identifier);
        expect(found.name).toBe(tag.name);
      });

      it("既存のタグを更新できる", async () => {
        const repository = createRepository();
        const tag = Builder(TagMold).buildWith(2);

        await repository.persist(tag).unwrap();

        const newName = Builder(TagNameMold).buildWith(100, {
          value: "UpdatedTag",
        });
        const updatedTag = Builder(TagMold).duplicate(tag, {
          name: newName,
        });

        const result = await repository.persist(updatedTag).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(tag.identifier).unwrap();
        expect(found.name).toBe(newName);
      });

      it("存在しないタグを更新しようとするとエラーになる", async () => {
        const repository = createRepository();
        const tag = Builder(TagMold).buildWith(3);

        await repository.persist(tag).unwrap();

        clearFirestore(testEnvironment.firestore);

        const newName = Builder(TagNameMold).buildWith(101, {
          value: "UpdatedTag2",
        });
        const updatedTag = Builder(TagMold).duplicate(tag, {
          name: newName,
        });

        const error = await extractError(repository.persist(updatedTag));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });

      it("重複する名前でタグを作成しようとするとエラーになる", async () => {
        const repository = createRepository();
        const name = Builder(TagNameMold).buildWith(4, {
          value: "DuplicateName",
        });
        const firstTag = Builder(TagMold).buildWith(5, { name });

        await repository.persist(firstTag).unwrap();

        const secondTag = Builder(TagMold).buildWith(6, { name });

        const error = await extractError(repository.persist(secondTag));

        expect(error).not.toBeNull();
        expect(isDuplicationError(error)).toBe(true);
      });
    });

    describe("find", () => {
      it("存在するタグを取得できる", async () => {
        const repository = createRepository();
        const tag = Builder(TagMold).buildWith(10);

        await repository.persist(tag).unwrap();

        const found = await repository.find(tag.identifier).unwrap();

        expect(found.identifier).toBe(tag.identifier);
        expect(found.name).toBe(tag.name);
        expect(found.logo).toBe(tag.logo);
      });

      it("存在しないタグを取得しようとするとエラーになる", async () => {
        const repository = createRepository();
        const identifier = Builder(TagIdentifierMold).buildWith(11);

        const error = await extractError(repository.find(identifier));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });
    });

    describe("search", () => {
      it("全てのタグを取得できる", async () => {
        const repository = createRepository();
        const tags = Builder(TagMold).buildListWith(5, 20).toArray();

        for (const tag of tags) {
          await repository.persist(tag).unwrap();
        }

        const criteria = createCriteria({});
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(5);
      });

      it("nameで部分検索できる", async () => {
        const repository = createRepository();
        const typeScriptName = Builder(TagNameMold).buildWith(30, {
          value: "TypeScript",
        });
        const pythonName = Builder(TagNameMold).buildWith(31, {
          value: "Python",
        });
        const typeScriptTag = Builder(TagMold).buildWith(32, {
          name: typeScriptName,
        });
        const pythonTag = Builder(TagMold).buildWith(33, { name: pythonName });

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
        const tags = Builder(TagMold).buildListWith(3, 40).toArray();

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
        const tag = Builder(TagMold).buildWith(50);

        await repository.persist(tag).unwrap();

        const nonExistentIdentifier = Builder(TagIdentifierMold).buildWith(51);
        const identifiers = [tag.identifier, nonExistentIdentifier];

        const found = await repository.ofIdentifiers(identifiers).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.identifier).toBe(tag.identifier);
      });
    });

    describe("ofNames", () => {
      it("名前でタグを取得できる", async () => {
        const repository = createRepository();
        const reactName = Builder(TagNameMold).buildWith(60, { value: "React" });
        const vueName = Builder(TagNameMold).buildWith(61, { value: "Vue" });
        const reactTag = Builder(TagMold).buildWith(62, { name: reactName });
        const vueTag = Builder(TagMold).buildWith(63, { name: vueName });

        await repository.persist(reactTag).unwrap();
        await repository.persist(vueTag).unwrap();

        const found = await repository.ofNames([reactName, vueName]).unwrap();

        expect(found.length).toBe(2);
      });

      it("存在しない名前はスキップされる", async () => {
        const repository = createRepository();
        const angularName = Builder(TagNameMold).buildWith(70, {
          value: "Angular",
        });
        const svelteName = Builder(TagNameMold).buildWith(71, {
          value: "Svelte",
        });
        const angularTag = Builder(TagMold).buildWith(72, { name: angularName });

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
        const name = Builder(TagNameMold).buildWith(80, {
          value: "DuplicateName",
        });
        const firstTag = Builder(TagMold).buildWith(81, { name });
        const secondTag = Builder(TagMold).buildWith(82, { name });

        await repository.persist(firstTag).unwrap();

        const error = await extractError(repository.persist(secondTag));

        expect(error).not.toBeNull();
        expect(isDuplicationError(error)).toBe(true);
      });

      it("タグ更新時に他のタグと同じ名前に変更しようとするとエラーになる", async () => {
        const repository = createRepository();
        const firstName = Builder(TagNameMold).buildWith(90, {
          value: "NameOne",
        });
        const secondName = Builder(TagNameMold).buildWith(91, {
          value: "NameTwo",
        });

        const firstTag = Builder(TagMold).buildWith(92, { name: firstName });
        const secondTag = Builder(TagMold).buildWith(93, { name: secondName });

        await repository.persist(firstTag).unwrap();
        await repository.persist(secondTag).unwrap();

        const updatedSecondTag = Builder(TagMold).duplicate(secondTag, {
          name: firstName,
        });

        const error = await extractError(repository.persist(updatedSecondTag));

        expect(error).not.toBeNull();
        expect(isDuplicationError(error)).toBe(true);
      });

      it("タグ削除後に同じ名前で新しいタグを作成できる", async () => {
        const repository = createRepository();
        const name = Builder(TagNameMold).buildWith(100, {
          value: "ReusableName",
        });
        const firstTag = Builder(TagMold).buildWith(101, { name });

        await repository.persist(firstTag).unwrap();
        await repository.terminate(firstTag.identifier).unwrap();

        const secondTag = Builder(TagMold).buildWith(102, { name });
        const result = await repository.persist(secondTag).unwrap();

        expect(result).toBeUndefined();
      });

      it("同じタグの名前を変更できる", async () => {
        const repository = createRepository();
        const oldName = Builder(TagNameMold).buildWith(110, {
          value: "OldName",
        });
        const newName = Builder(TagNameMold).buildWith(111, {
          value: "NewName",
        });

        const tag = Builder(TagMold).buildWith(112, { name: oldName });

        await repository.persist(tag).unwrap();

        const updatedTag = Builder(TagMold).duplicate(tag, { name: newName });

        const result = await repository.persist(updatedTag).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(tag.identifier).unwrap();
        expect(found.name).toBe(newName);
      });
    });

    describe("terminate", () => {
      it("タグを削除できる", async () => {
        const repository = createRepository();
        const tag = Builder(TagMold).buildWith(120);

        await repository.persist(tag).unwrap();

        const result = await repository.terminate(tag.identifier).unwrap();

        expect(result).toBeUndefined();

        const error = await extractError(repository.find(tag.identifier));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });

      it("存在しないタグを削除しようとするとエラーになる", async () => {
        const repository = createRepository();
        const identifier = Builder(TagIdentifierMold).buildWith(130);

        const error = await extractError(repository.terminate(identifier));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });
    });
  });
});
