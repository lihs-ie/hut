import { describe, it, expect } from "vitest";
import { Builder } from "../support/molds";
import {
  SearchTokenMold,
  SearchTokenIdentifierMold,
  SearchReferenceMold,
  SearchTokenCriteriaMold,
  SearchTokenRepositoryMold,
} from "../support/molds/domains/search-token/common";
import { isAggregateNotFoundError } from "@shared/aspects/error";
import { Forger } from "@lihs-ie/forger-ts";
import { extractError } from "../support/helpers";
import type { SearchToken } from "@shared/domains/search-token";

describe("infrastructures/search-token (with mock repository)", () => {
  const createRepository = (tokens: SearchToken[] = []) =>
    Forger(SearchTokenRepositoryMold).forgeWithSeed(1, { instances: tokens });

  describe("SearchTokenRepository", () => {
    describe("persist", () => {
      it("新しい検索トークンを保存できる", async () => {
        const repository = createRepository();
        const token = Builder(SearchTokenMold).buildWith(1);

        const result = await repository.persist(token).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(token.identifier).unwrap();
        expect(found.identifier).toBe(token.identifier);
        expect(found.value).toBe(token.value);
        expect(found.type).toBe(token.type);
      });

      it("既存の検索トークンを更新できる", async () => {
        const token = Builder(SearchTokenMold).buildWith(2);
        const repository = createRepository([token]);

        const newReferences = Builder(SearchReferenceMold)
          .buildListWith(5, 100)
          .toArray();
        const updatedToken = Builder(SearchTokenMold).duplicate(token, {
          references: newReferences,
        });

        const result = await repository.persist(updatedToken).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(token.identifier).unwrap();
        expect(found.references.length).toBe(5);
      });

      it("参照情報も一緒に保存できる", async () => {
        const references = Builder(SearchReferenceMold)
          .buildListWith(3, 10)
          .toArray();
        const token = Builder(SearchTokenMold).buildWith(3, { references });
        const repository = createRepository();

        const result = await repository.persist(token).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(token.identifier).unwrap();
        expect(found.references.length).toBe(3);
      });

      it("参照情報を更新できる", async () => {
        const initialReferences = Builder(SearchReferenceMold)
          .buildListWith(2, 20)
          .toArray();
        const token = Builder(SearchTokenMold).buildWith(4, {
          references: initialReferences,
        });
        const repository = createRepository([token]);

        const newReferences = Builder(SearchReferenceMold)
          .buildListWith(4, 30)
          .toArray();
        const updatedToken = Builder(SearchTokenMold).duplicate(token, {
          references: newReferences,
        });

        await repository.persist(updatedToken).unwrap();

        const found = await repository.find(token.identifier).unwrap();
        expect(found.references.length).toBe(4);
      });

      it("参照情報を削除できる", async () => {
        const initialReferences = Builder(SearchReferenceMold)
          .buildListWith(3, 40)
          .toArray();
        const token = Builder(SearchTokenMold).buildWith(5, {
          references: initialReferences,
        });
        const repository = createRepository([token]);

        const updatedToken = Builder(SearchTokenMold).duplicate(token, {
          references: [],
        });

        await repository.persist(updatedToken).unwrap();

        const found = await repository.find(token.identifier).unwrap();
        expect(found.references.length).toBe(0);
      });
    });

    describe("find", () => {
      it("存在する検索トークンを取得できる", async () => {
        const token = Builder(SearchTokenMold).buildWith(10);
        const repository = createRepository([token]);

        const found = await repository.find(token.identifier).unwrap();

        expect(found.identifier).toBe(token.identifier);
        expect(found.value).toBe(token.value);
        expect(found.type).toBe(token.type);
      });

      it("存在しない検索トークンを取得しようとするとエラーになる", async () => {
        const repository = createRepository();
        const identifier = Builder(SearchTokenIdentifierMold).buildWith(11);

        const error = await extractError(repository.find(identifier));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });

      it("参照情報も含めて取得できる", async () => {
        const references = Builder(SearchReferenceMold)
          .buildListWith(2, 50)
          .toArray();
        const token = Builder(SearchTokenMold).buildWith(12, { references });
        const repository = createRepository([token]);

        const found = await repository.find(token.identifier).unwrap();

        expect(found.references.length).toBe(2);
      });
    });

    describe("ofIdentifiers", () => {
      it("複数の検索トークンを取得できる", async () => {
        const tokens = Builder(SearchTokenMold).buildListWith(3, 20).toArray();
        const repository = createRepository(tokens);

        const identifiers = tokens.map((token) => token.identifier);
        const found = await repository.ofIdentifiers(identifiers).unwrap();

        expect(found.length).toBe(3);
      });

      it("空の配列を渡すと空の配列を返す", async () => {
        const repository = createRepository();

        const found = await repository.ofIdentifiers([]).unwrap();

        expect(found.length).toBe(0);
      });

      it("存在しない検索トークンはスキップされる (throwOnMissing=false)", async () => {
        const token = Builder(SearchTokenMold).buildWith(30);
        const repository = createRepository([token]);

        const nonExistentIdentifier =
          Builder(SearchTokenIdentifierMold).buildWith(31);
        const identifiers = [token.identifier, nonExistentIdentifier];

        const found = await repository
          .ofIdentifiers(identifiers, false)
          .unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.identifier).toBe(token.identifier);
      });

      it("存在しない検索トークンがあるとエラーになる (throwOnMissing=true)", async () => {
        const token = Builder(SearchTokenMold).buildWith(35);
        const repository = createRepository([token]);

        const nonExistentIdentifier =
          Builder(SearchTokenIdentifierMold).buildWith(36);
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
        const tokens = Builder(SearchTokenMold).buildListWith(5, 40).toArray();
        const repository = createRepository(tokens);

        const criteria = Builder(SearchTokenCriteriaMold).buildWith(50);
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(5);
      });

      it("参照情報も含めて検索結果に含まれる", async () => {
        const references = Builder(SearchReferenceMold)
          .buildListWith(2, 80)
          .toArray();
        const token = Builder(SearchTokenMold).buildWith(81, { references });
        const repository = createRepository([token]);

        const criteria = Builder(SearchTokenCriteriaMold).buildWith(82);
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.references.length).toBe(2);
      });
    });

    describe("terminate", () => {
      it("検索トークンを削除できる", async () => {
        const token = Builder(SearchTokenMold).buildWith(100);
        const repository = createRepository([token]);

        const result = await repository.terminate(token.identifier).unwrap();

        expect(result).toBeUndefined();

        const error = await extractError(repository.find(token.identifier));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });

      it("参照情報も一緒に削除される", async () => {
        const references = Builder(SearchReferenceMold)
          .buildListWith(3, 110)
          .toArray();
        const token = Builder(SearchTokenMold).buildWith(111, { references });
        const repository = createRepository([token]);

        const result = await repository.terminate(token.identifier).unwrap();

        expect(result).toBeUndefined();

        const error = await extractError(repository.find(token.identifier));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });

      it("存在しない検索トークンを削除しようとするとエラーになる", async () => {
        const repository = createRepository();
        const identifier = Builder(SearchTokenIdentifierMold).buildWith(120);

        const error = await extractError(repository.terminate(identifier));

        expect(error).not.toBeNull();
        expect(isAggregateNotFoundError(error)).toBe(true);
      });
    });
  });
});
