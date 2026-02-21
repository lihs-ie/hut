import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SearchTokenMold,
  SearchTokenIdentifierMold,
  SearchReferenceMold,
  SearchTokenCriteriaMold,
  SearchTokenRepositoryMold,
} from "../support/molds/domains/search-token/common";
import { isAggregateNotFoundError } from "@shared/aspects/error";
import { extractError } from "../support/helpers";

describe("infrastructures/search-token (with mock repository)", () => {
  describe("SearchTokenRepository", () => {
    describe("persist", () => {
      it("新しい検索トークンを保存できる", async () => {
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [] });
        const token = Forger(SearchTokenMold).forgeWithSeed(1);

        const result = await repository.persist(token).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(token.identifier).unwrap();
        expect(found.identifier).toBe(token.identifier);
        expect(found.value).toBe(token.value);
        expect(found.type).toBe(token.type);
      });

      it("既存の検索トークンを更新できる", async () => {
        const token = Forger(SearchTokenMold).forgeWithSeed(2);
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [token] });

        const newReferences = Forger(SearchReferenceMold).forgeMultiWithSeed(5, 100);
        const updatedToken = Forger(SearchTokenMold).forgeWithSeed(0, {
          ...token,
          references: newReferences,
        });

        const result = await repository.persist(updatedToken).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(token.identifier).unwrap();
        expect(found.references.length).toBe(5);
      });

      it("参照情報も一緒に保存できる", async () => {
        const references = Forger(SearchReferenceMold).forgeMultiWithSeed(3, 10);
        const token = Forger(SearchTokenMold).forgeWithSeed(3, { references });
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [] });

        const result = await repository.persist(token).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(token.identifier).unwrap();
        expect(found.references.length).toBe(3);
      });

      it("参照情報を更新できる", async () => {
        const initialReferences = Forger(SearchReferenceMold).forgeMultiWithSeed(2, 20);
        const token = Forger(SearchTokenMold).forgeWithSeed(4, {
          references: initialReferences,
        });
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [token] });

        const newReferences = Forger(SearchReferenceMold).forgeMultiWithSeed(4, 30);
        const updatedToken = Forger(SearchTokenMold).forgeWithSeed(0, {
          ...token,
          references: newReferences,
        });

        await repository.persist(updatedToken).unwrap();

        const found = await repository.find(token.identifier).unwrap();
        expect(found.references.length).toBe(4);
      });

      it("参照情報を削除できる", async () => {
        const initialReferences = Forger(SearchReferenceMold).forgeMultiWithSeed(3, 40);
        const token = Forger(SearchTokenMold).forgeWithSeed(5, {
          references: initialReferences,
        });
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [token] });

        const updatedToken = Forger(SearchTokenMold).forgeWithSeed(0, {
          ...token,
          references: [],
        });

        await repository.persist(updatedToken).unwrap();

        const found = await repository.find(token.identifier).unwrap();
        expect(found.references.length).toBe(0);
      });
    });

    describe("find", () => {
      it("存在する検索トークンを取得できる", async () => {
        const token = Forger(SearchTokenMold).forgeWithSeed(10);
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [token] });

        const found = await repository.find(token.identifier).unwrap();

        expect(found.identifier).toBe(token.identifier);
        expect(found.value).toBe(token.value);
        expect(found.type).toBe(token.type);
      });

      it("存在しない検索トークンを取得しようとするとエラーになる", async () => {
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [] });
        const identifier = Forger(SearchTokenIdentifierMold).forgeWithSeed(11);

        const error = await extractError(repository.find(identifier));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });

      it("参照情報も含めて取得できる", async () => {
        const references = Forger(SearchReferenceMold).forgeMultiWithSeed(2, 50);
        const token = Forger(SearchTokenMold).forgeWithSeed(12, { references });
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [token] });

        const found = await repository.find(token.identifier).unwrap();

        expect(found.references.length).toBe(2);
      });
    });

    describe("ofIdentifiers", () => {
      it("複数の検索トークンを取得できる", async () => {
        const tokens = Forger(SearchTokenMold).forgeMultiWithSeed(3, 20);
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: tokens });

        const identifiers = tokens.map((token) => token.identifier);
        const found = await repository.ofIdentifiers(identifiers).unwrap();

        expect(found.length).toBe(3);
      });

      it("空の配列を渡すと空の配列を返す", async () => {
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [] });

        const found = await repository.ofIdentifiers([]).unwrap();

        expect(found.length).toBe(0);
      });

      it("存在しない検索トークンはスキップされる (throwOnMissing=false)", async () => {
        const token = Forger(SearchTokenMold).forgeWithSeed(30);
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [token] });

        const nonExistentIdentifier =
          Forger(SearchTokenIdentifierMold).forgeWithSeed(31);
        const identifiers = [token.identifier, nonExistentIdentifier];

        const found = await repository
          .ofIdentifiers(identifiers, false)
          .unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.identifier).toBe(token.identifier);
      });

      it("存在しない検索トークンがあるとエラーになる (throwOnMissing=true)", async () => {
        const token = Forger(SearchTokenMold).forgeWithSeed(35);
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [token] });

        const nonExistentIdentifier =
          Forger(SearchTokenIdentifierMold).forgeWithSeed(36);
        const identifiers = [token.identifier, nonExistentIdentifier];

        const error = await extractError(
          repository.ofIdentifiers(identifiers, true),
        );

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });
    });

    describe("search", () => {
      it("検索トークンを検索できる", async () => {
        const tokens = Forger(SearchTokenMold).forgeMultiWithSeed(5, 40);
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: tokens });

        const criteria = Forger(SearchTokenCriteriaMold).forgeWithSeed(50);
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(5);
      });

      it("参照情報も含めて検索結果に含まれる", async () => {
        const references = Forger(SearchReferenceMold).forgeMultiWithSeed(2, 80);
        const token = Forger(SearchTokenMold).forgeWithSeed(81, { references });
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [token] });

        const criteria = Forger(SearchTokenCriteriaMold).forgeWithSeed(82);
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.references.length).toBe(2);
      });
    });

    describe("terminate", () => {
      it("検索トークンを削除できる", async () => {
        const token = Forger(SearchTokenMold).forgeWithSeed(100);
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [token] });

        const result = await repository.terminate(token.identifier).unwrap();

        expect(result).toBeUndefined();

        const error = await extractError(repository.find(token.identifier));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });

      it("参照情報も一緒に削除される", async () => {
        const references = Forger(SearchReferenceMold).forgeMultiWithSeed(3, 110);
        const token = Forger(SearchTokenMold).forgeWithSeed(111, { references });
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [token] });

        const result = await repository.terminate(token.identifier).unwrap();

        expect(result).toBeUndefined();

        const error = await extractError(repository.find(token.identifier));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });

      it("存在しない検索トークンを削除しようとするとエラーになる", async () => {
        const repository = Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: [] });
        const identifier = Forger(SearchTokenIdentifierMold).forgeWithSeed(120);

        const error = await extractError(repository.terminate(identifier));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });
    });
  });
});
