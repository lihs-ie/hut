import { describe, it, expect, beforeEach } from "vitest";
import { FirebaseSiteDocumentRepository } from "@shared/infrastructures/documents";
import {
  createTestFirestoreWithSeed,
  clearFirestore,
  type Firestore,
} from "../support/mock/firebase/firestore";
import { isAggregateNotFoundError } from "@shared/aspects/error";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { validateSiteDocument, type SiteDocument } from "@shared/domains/document";

describe("infrastructures/documents", () => {
  let firestore: Firestore;
  let operations: unknown;

  beforeEach(async () => {
    const testEnv = await createTestFirestoreWithSeed({});
    firestore = testEnv.firestore;
    operations = testEnv.operations;
  });

  const getOperations = () => operations as FirestoreOperations;

  const createSiteDocument = (seed: number): SiteDocument => {
    const now = new Date();
    return validateSiteDocument({
      privacy: {
        sections: [
          {
            headline: `プライバシーポリシーセクション${seed}`,
            body: `これはセクション${seed}の本文です。プライバシーに関する重要な情報が記載されています。`,
            list: [`項目${seed}-1`, `項目${seed}-2`, `項目${seed}-3`],
          },
          {
            headline: `追加セクション${seed}`,
            body: `追加のセクション本文${seed}`,
            list: null,
          },
        ],
        timeline: {
          createdAt: now,
          updatedAt: now,
        },
      },
    }).unwrap();
  };

  describe("FirebaseSiteDocumentRepository", () => {
    describe("persist", () => {
      it("新しいサイトドキュメントを保存できる", async () => {
        const repository = FirebaseSiteDocumentRepository(
          firestore,
          getOperations()
        );
        const document = createSiteDocument(1);

        const result = await repository.persist(document).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find().unwrap();
        expect(found.privacy.sections.length).toBe(
          document.privacy.sections.length
        );
        expect(found.privacy.sections[0]?.headline).toBe(
          document.privacy.sections[0]?.headline
        );
      });

      it("既存のサイトドキュメントを更新できる", async () => {
        const repository = FirebaseSiteDocumentRepository(
          firestore,
          getOperations()
        );
        const document = createSiteDocument(2);

        await repository.persist(document).unwrap();

        const updatedDocument = validateSiteDocument({
          privacy: {
            sections: [
              {
                headline: "更新されたセクション",
                body: "更新された本文内容",
                list: ["更新項目1", "更新項目2"],
              },
            ],
            timeline: {
              createdAt: document.privacy.timeline.createdAt,
              updatedAt: new Date(),
            },
          },
        }).unwrap();

        const result = await repository.persist(updatedDocument).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find().unwrap();
        expect(found.privacy.sections[0]?.headline).toBe("更新されたセクション");
      });

      it("存在しないドキュメントを更新しようとするとエラーになる", async () => {
        const repository = FirebaseSiteDocumentRepository(
          firestore,
          getOperations()
        );
        const document = createSiteDocument(3);

        await repository.persist(document).unwrap();

        clearFirestore(firestore);

        const updatedDocument = validateSiteDocument({
          privacy: {
            sections: [
              {
                headline: "更新セクション",
                body: "更新本文",
                list: null,
              },
            ],
            timeline: {
              createdAt: document.privacy.timeline.createdAt,
              updatedAt: new Date(),
            },
          },
        }).unwrap();

        const result = await repository.persist(updatedDocument).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("find", () => {
      it("存在するサイトドキュメントを取得できる", async () => {
        const repository = FirebaseSiteDocumentRepository(
          firestore,
          getOperations()
        );
        const document = createSiteDocument(10);

        await repository.persist(document).unwrap();

        const found = await repository.find().unwrap();

        expect(found.privacy.sections.length).toBe(
          document.privacy.sections.length
        );
        expect(found.privacy.sections[0]?.headline).toBe(
          document.privacy.sections[0]?.headline
        );
        expect(found.privacy.sections[0]?.body).toBe(
          document.privacy.sections[0]?.body
        );
        expect(found.privacy.sections[0]?.list).toEqual(
          document.privacy.sections[0]?.list
        );
      });

      it("存在しないサイトドキュメントを取得しようとするとエラーになる", async () => {
        const repository = FirebaseSiteDocumentRepository(
          firestore,
          getOperations()
        );

        const result = await repository.find().match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });

      it("セクションにlistがnullの場合も正しく取得できる", async () => {
        const repository = FirebaseSiteDocumentRepository(
          firestore,
          getOperations()
        );
        const document = validateSiteDocument({
          privacy: {
            sections: [
              {
                headline: "リストなしセクション",
                body: "リストを持たないセクションの本文です",
                list: null,
              },
            ],
            timeline: {
              createdAt: new Date(),
              updatedAt: new Date(),
            },
          },
        }).unwrap();

        await repository.persist(document).unwrap();

        const found = await repository.find().unwrap();

        expect(found.privacy.sections[0]?.list).toBeNull();
      });

      it("複数のセクションを持つドキュメントを正しく取得できる", async () => {
        const repository = FirebaseSiteDocumentRepository(
          firestore,
          getOperations()
        );
        const document = validateSiteDocument({
          privacy: {
            sections: [
              {
                headline: "セクション1",
                body: "セクション1の本文",
                list: ["項目1", "項目2"],
              },
              {
                headline: "セクション2",
                body: "セクション2の本文",
                list: ["項目A", "項目B", "項目C"],
              },
              {
                headline: "セクション3",
                body: "セクション3の本文",
                list: null,
              },
            ],
            timeline: {
              createdAt: new Date(),
              updatedAt: new Date(),
            },
          },
        }).unwrap();

        await repository.persist(document).unwrap();

        const found = await repository.find().unwrap();

        expect(found.privacy.sections.length).toBe(3);
        expect(found.privacy.sections[0]?.headline).toBe("セクション1");
        expect(found.privacy.sections[1]?.headline).toBe("セクション2");
        expect(found.privacy.sections[2]?.headline).toBe("セクション3");
      });
    });
  });
});
