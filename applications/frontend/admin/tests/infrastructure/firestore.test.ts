import { describe, it, expect, vi, beforeEach } from "vitest";
import { Timestamp as AdminTimestamp } from "firebase-admin/firestore";
import { Timestamp as ClientTimestamp } from "firebase/firestore";
import { createAdminFirestoreAdapter } from "@/infrastructure/firestore";
import type { AdminFirestoreCompatible } from "@/infrastructure/firestore";

type MockDocumentSnapshot = {
  exists: boolean;
  data: () => Record<string, unknown>;
  id: string;
  ref: unknown;
  get: (fieldPath: string) => unknown;
};

type MockDocumentRef = {
  get: () => Promise<MockDocumentSnapshot>;
  set: (data: unknown, options?: unknown) => Promise<unknown>;
  update: (data: unknown) => Promise<unknown>;
  delete: () => Promise<unknown>;
  collection: ReturnType<typeof vi.fn>;
  withConverter: ReturnType<typeof vi.fn>;
  id: string;
  path: string;
  doc: ReturnType<typeof vi.fn>;
};

type MockCollectionRef = {
  doc: ReturnType<typeof vi.fn>;
  add: ReturnType<typeof vi.fn>;
  withConverter: ReturnType<typeof vi.fn>;
  get: ReturnType<typeof vi.fn>;
  where: ReturnType<typeof vi.fn>;
  orderBy: ReturnType<typeof vi.fn>;
};

type MockQuery = {
  where: ReturnType<typeof vi.fn>;
  orderBy: ReturnType<typeof vi.fn>;
  limit: ReturnType<typeof vi.fn>;
  limitToLast: ReturnType<typeof vi.fn>;
  startAt: ReturnType<typeof vi.fn>;
  startAfter: ReturnType<typeof vi.fn>;
  endAt: ReturnType<typeof vi.fn>;
  endBefore: ReturnType<typeof vi.fn>;
  get: ReturnType<typeof vi.fn>;
};

type MockQuerySnapshot = {
  docs: MockDocumentSnapshot[];
  size: number;
  empty: boolean;
};

type MockTransaction = {
  get: ReturnType<typeof vi.fn>;
  set: ReturnType<typeof vi.fn>;
  update: ReturnType<typeof vi.fn>;
  delete: ReturnType<typeof vi.fn>;
};

const createMockSnapshot = (
  exists: boolean,
  data: Record<string, unknown> = {},
  id = "doc-1",
): MockDocumentSnapshot => ({
  exists,
  data: () => data,
  id,
  ref: {},
  get: (fieldPath: string) => data[fieldPath],
});

const createMockQuery = (): MockQuery => {
  const query: MockQuery = {
    where: vi.fn(),
    orderBy: vi.fn(),
    limit: vi.fn(),
    limitToLast: vi.fn(),
    startAt: vi.fn(),
    startAfter: vi.fn(),
    endAt: vi.fn(),
    endBefore: vi.fn(),
    get: vi.fn().mockResolvedValue({ docs: [], size: 0, empty: true }),
  };
  query.where.mockReturnValue(query);
  query.orderBy.mockReturnValue(query);
  query.limit.mockReturnValue(query);
  query.limitToLast.mockReturnValue(query);
  query.startAt.mockReturnValue(query);
  query.startAfter.mockReturnValue(query);
  query.endAt.mockReturnValue(query);
  query.endBefore.mockReturnValue(query);
  return query;
};

const createMockDocumentRef = (
  snapshot?: MockDocumentSnapshot,
): MockDocumentRef => {
  const ref: MockDocumentRef = {
    get: vi.fn().mockResolvedValue(snapshot ?? createMockSnapshot(false)),
    set: vi.fn().mockResolvedValue(undefined),
    update: vi.fn().mockResolvedValue(undefined),
    delete: vi.fn().mockResolvedValue(undefined),
    collection: vi.fn(),
    withConverter: vi.fn(),
    id: "doc-1",
    path: "collection/doc-1",
    doc: vi.fn(),
  };
  ref.withConverter.mockReturnValue(ref);
  ref.doc.mockReturnValue(ref);
  return ref;
};

const createMockCollectionRef = (): MockCollectionRef => {
  const mockDocRef = createMockDocumentRef();
  const collectionRef: MockCollectionRef = {
    doc: vi.fn().mockReturnValue(mockDocRef),
    add: vi.fn().mockResolvedValue(mockDocRef),
    withConverter: vi.fn(),
    get: vi.fn().mockResolvedValue({ docs: [], size: 0, empty: true }),
    where: vi.fn().mockReturnValue(createMockQuery()),
    orderBy: vi.fn().mockReturnValue(createMockQuery()),
  };
  collectionRef.withConverter.mockReturnValue(collectionRef);
  return collectionRef;
};

const createMockAdminFirestore = (): {
  firestore: AdminFirestoreCompatible;
  mockCollection: ReturnType<typeof vi.fn>;
  mockDoc: ReturnType<typeof vi.fn>;
  mockCollectionGroup: ReturnType<typeof vi.fn>;
  mockRunTransaction: ReturnType<typeof vi.fn>;
  mockBatch: ReturnType<typeof vi.fn>;
} => {
  const mockCollection = vi.fn().mockReturnValue(createMockCollectionRef());
  const mockDoc = vi.fn().mockReturnValue(createMockDocumentRef());
  const mockCollectionGroup = vi.fn().mockReturnValue(createMockQuery());
  const mockRunTransaction = vi.fn();
  const mockBatch = vi.fn().mockReturnValue({
    set: vi.fn(),
    update: vi.fn(),
    delete: vi.fn(),
    commit: vi.fn().mockResolvedValue(undefined),
  });

  return {
    firestore: {
      collection: mockCollection,
      doc: mockDoc,
      collectionGroup: mockCollectionGroup,
      runTransaction: mockRunTransaction,
      batch: mockBatch,
    },
    mockCollection,
    mockDoc,
    mockCollectionGroup,
    mockRunTransaction,
    mockBatch,
  };
};

describe("infrastructure/firestore-adapter", () => {
  let mockAdminFirestore: ReturnType<typeof createMockAdminFirestore>;

  beforeEach(() => {
    vi.clearAllMocks();
    mockAdminFirestore = createMockAdminFirestore();
  });

  describe("createAdminFirestoreAdapter", () => {
    it("instance と operations を持つオブジェクトを返す", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      expect(adapter).toHaveProperty("instance");
      expect(adapter).toHaveProperty("operations");
    });

    it("instance が渡された AdminFirestore インスタンスである", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      expect(adapter.instance).toBeDefined();
    });
  });

  describe("operations.collection", () => {
    it("単一パスでコレクションを取得する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      adapter.operations.collection(
        adapter.instance,
        "articles",
      );

      expect(mockAdminFirestore.mockCollection).toHaveBeenCalledWith(
        "articles",
      );
    });

    it("複数パスセグメントを結合してコレクションを取得する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      adapter.operations.collection(
        adapter.instance,
        "articles",
        "doc-1",
        "comments",
      );

      expect(mockAdminFirestore.mockCollection).toHaveBeenCalledWith(
        "articles/doc-1/comments",
      );
    });
  });

  describe("operations.doc", () => {
    it("CollectionReference 上で doc を呼び出す", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );
      const mockCollectionRef = createMockCollectionRef();

      adapter.operations.doc(mockCollectionRef, "doc-1");

      expect(mockCollectionRef.doc).toHaveBeenCalledWith("doc-1");
    });

    it("パスが省略された場合にリファレンスをそのまま返す", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );
      const mockRef = createMockDocumentRef();

      const result = adapter.operations.doc(mockRef);

      expect(result).toBe(mockRef);
    });

    it("複数パスセグメントを結合する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );
      const mockCollectionRef = createMockCollectionRef();

      adapter.operations.doc(mockCollectionRef, "path1", "path2");

      expect(mockCollectionRef.doc).toHaveBeenCalledWith("path1/path2");
    });

    it("doc メソッドを持たないリファレンスの場合は adminFirestore.doc を使用する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );
      const refWithoutDoc = { id: "ref-1", path: "some/path" };

      adapter.operations.doc(refWithoutDoc, "some/path");

      expect(mockAdminFirestore.mockDoc).toHaveBeenCalledWith("some/path");
    });
  });

  describe("operations.getDoc", () => {
    it("snapshot.exists() をメソッドとして返す (Admin SDK の exists はプロパティ)", async () => {
      const mockSnapshot = createMockSnapshot(true, { title: "test" });
      const mockRef = createMockDocumentRef(mockSnapshot);

      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const result = await adapter.operations.getDoc(mockRef);

      expect(typeof result.exists).toBe("function");
      expect(result.exists()).toBe(true);
    });

    it("存在しないドキュメントで exists() が false を返す", async () => {
      const mockSnapshot = createMockSnapshot(false);
      const mockRef = createMockDocumentRef(mockSnapshot);

      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const result = await adapter.operations.getDoc(mockRef);

      expect(result.exists()).toBe(false);
    });

    it("data() メソッドがそのまま機能する", async () => {
      const testData = { title: "test", content: "hello" };
      const mockSnapshot = createMockSnapshot(true, testData);
      const mockRef = createMockDocumentRef(mockSnapshot);

      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const result = await adapter.operations.getDoc(mockRef);

      expect(result.data()).toEqual(testData);
    });
  });

  describe("operations.getDocs", () => {
    it("各ドキュメントの exists をメソッドとしてラップする", async () => {
      const mockSnapshots = [
        createMockSnapshot(true, { title: "doc1" }, "id-1"),
        createMockSnapshot(true, { title: "doc2" }, "id-2"),
      ];
      const mockQuerySnapshot: MockQuerySnapshot = {
        docs: mockSnapshots,
        size: 2,
        empty: false,
      };
      const mockQueryRef = {
        get: vi.fn().mockResolvedValue(mockQuerySnapshot),
      };

      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const result = await adapter.operations.getDocs(mockQueryRef);

      expect(result.docs).toHaveLength(2);
      result.docs.forEach((doc: { exists: () => boolean }) => {
        expect(typeof doc.exists).toBe("function");
        expect(doc.exists()).toBe(true);
      });
    });

    it("forEach がラップされたドキュメントで動作する", async () => {
      const mockSnapshots = [
        createMockSnapshot(true, { title: "doc1" }, "id-1"),
      ];
      const mockQuerySnapshot: MockQuerySnapshot = {
        docs: mockSnapshots,
        size: 1,
        empty: false,
      };
      const mockQueryRef = {
        get: vi.fn().mockResolvedValue(mockQuerySnapshot),
      };

      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const result = await adapter.operations.getDocs(mockQueryRef);
      const collected: unknown[] = [];
      result.forEach((doc: { exists: () => boolean }) => {
        collected.push(doc);
        expect(typeof doc.exists).toBe("function");
      });

      expect(collected).toHaveLength(1);
    });

    it("空のクエリ結果を処理する", async () => {
      const mockQuerySnapshot: MockQuerySnapshot = {
        docs: [],
        size: 0,
        empty: true,
      };
      const mockQueryRef = {
        get: vi.fn().mockResolvedValue(mockQuerySnapshot),
      };

      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const result = await adapter.operations.getDocs(mockQueryRef);

      expect(result.docs).toHaveLength(0);
      expect(result.empty).toBe(true);
      expect(result.size).toBe(0);
    });
  });

  describe("constraint descriptors", () => {
    it("where constraint を生成する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const constraint = adapter.operations.where("status", "==", "published");

      expect(constraint).toHaveProperty("_type", "where");
      expect(constraint).toHaveProperty("_args", [
        "status",
        "==",
        "published",
      ]);
    });

    it("orderBy constraint を生成する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const constraint = adapter.operations.orderBy("createdAt", "desc");

      expect(constraint).toHaveProperty("_type", "orderBy");
      expect(constraint).toHaveProperty("_args", ["createdAt", "desc"]);
    });

    it("limit constraint を生成する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const constraint = adapter.operations.limit(10);

      expect(constraint).toHaveProperty("_type", "limit");
      expect(constraint).toHaveProperty("_args", [10]);
    });

    it("limitToLast constraint を生成する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const constraint = adapter.operations.limitToLast(5);

      expect(constraint).toHaveProperty("_type", "limitToLast");
      expect(constraint).toHaveProperty("_args", [5]);
    });

    it("startAt constraint を生成する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const constraint = adapter.operations.startAt("value1", "value2");

      expect(constraint).toHaveProperty("_type", "startAt");
      expect(constraint).toHaveProperty("_args", ["value1", "value2"]);
    });

    it("startAfter constraint を生成する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const constraint = adapter.operations.startAfter("value1");

      expect(constraint).toHaveProperty("_type", "startAfter");
      expect(constraint).toHaveProperty("_args", ["value1"]);
    });

    it("endAt constraint を生成する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const constraint = adapter.operations.endAt("value1");

      expect(constraint).toHaveProperty("_type", "endAt");
      expect(constraint).toHaveProperty("_args", ["value1"]);
    });

    it("endBefore constraint を生成する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const constraint = adapter.operations.endBefore("value1");

      expect(constraint).toHaveProperty("_type", "endBefore");
      expect(constraint).toHaveProperty("_args", ["value1"]);
    });

    it("and constraint を生成する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );
      const where1 = adapter.operations.where("status", "==", "published");
      const where2 = adapter.operations.where("type", "==", "article");

      const constraint = adapter.operations.and(where1, where2);

      expect(constraint).toHaveProperty("_type", "and");
    });

    it("or constraint を生成する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );
      const where1 = adapter.operations.where("status", "==", "published");
      const where2 = adapter.operations.where("status", "==", "draft");

      const constraint = adapter.operations.or(where1, where2);

      expect(constraint).toHaveProperty("_type", "or");
    });
  });

  describe("operations.query", () => {
    it("constraint を適用してクエリを返す", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );
      const mockQuery = createMockQuery();

      const whereConstraint = adapter.operations.where(
        "status",
        "==",
        "published",
      );
      adapter.operations.query(mockQuery, whereConstraint);

      expect(mockQuery.where).toHaveBeenCalledWith(
        "status",
        "==",
        "published",
      );
    });

    it("複数の constraint を順番に適用する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );
      const mockQuery = createMockQuery();

      const whereConstraint = adapter.operations.where(
        "status",
        "==",
        "published",
      );
      const orderByConstraint = adapter.operations.orderBy("createdAt", "desc");
      const limitConstraint = adapter.operations.limit(10);

      adapter.operations.query(
        mockQuery,
        whereConstraint,
        orderByConstraint,
        limitConstraint,
      );

      expect(mockQuery.where).toHaveBeenCalledWith(
        "status",
        "==",
        "published",
      );
    });
  });

  describe("operations.setDoc", () => {
    it("データをドキュメントに設定する", async () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );
      const mockRef = createMockDocumentRef();
      const data = { title: "test" };

      await adapter.operations.setDoc(mockRef, data);

      expect(mockRef.set).toHaveBeenCalledWith(data);
    });

    it("オプション付きでデータを設定する", async () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );
      const mockRef = createMockDocumentRef();
      const data = { title: "test" };
      const options = { merge: true };

      await adapter.operations.setDoc(mockRef, data, options);

      expect(mockRef.set).toHaveBeenCalledWith(data, options);
    });
  });

  describe("operations.updateDoc", () => {
    it("ドキュメントを更新する", async () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );
      const mockRef = createMockDocumentRef();
      const data = { title: "updated" };

      await adapter.operations.updateDoc(mockRef, data);

      expect(mockRef.update).toHaveBeenCalledWith(data);
    });
  });

  describe("operations.deleteDoc", () => {
    it("ドキュメントを削除する", async () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );
      const mockRef = createMockDocumentRef();

      await adapter.operations.deleteDoc(mockRef);

      expect(mockRef.delete).toHaveBeenCalled();
    });
  });

  describe("operations.addDoc", () => {
    it("コレクションにドキュメントを追加する", async () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );
      const mockCollectionRef = createMockCollectionRef();
      const data = { title: "new doc" };

      await adapter.operations.addDoc(mockCollectionRef, data);

      expect(mockCollectionRef.add).toHaveBeenCalledWith(data);
    });
  });

  describe("operations.collectionGroup", () => {
    it("collectionGroup を呼び出す", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      adapter.operations.collectionGroup(adapter.instance, "comments");

      expect(mockAdminFirestore.mockCollectionGroup).toHaveBeenCalledWith(
        "comments",
      );
    });
  });

  describe("operations.runTransaction", () => {
    it("トランザクション内で snapshot.exists() がメソッドとして動作する", async () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const mockSnapshot = createMockSnapshot(true, { title: "test" });
      const mockTransaction: MockTransaction = {
        get: vi.fn().mockResolvedValue(mockSnapshot),
        set: vi.fn().mockReturnThis(),
        update: vi.fn().mockReturnThis(),
        delete: vi.fn().mockReturnThis(),
      };

      mockAdminFirestore.mockRunTransaction.mockImplementation(
        async (callback: (transaction: MockTransaction) => Promise<unknown>) => {
          return callback(mockTransaction);
        },
      );

      await adapter.operations.runTransaction(
        adapter.instance,
        async (transaction) => {
          const mockRef = createMockDocumentRef();
          const snapshot = await transaction.get(mockRef);
          expect(typeof snapshot.exists).toBe("function");
          expect(snapshot.exists()).toBe(true);
          return snapshot.data();
        },
      );
    });
  });

  describe("operations.writeBatch", () => {
    it("batch オブジェクトを返す", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const batch = adapter.operations.writeBatch(adapter.instance);

      expect(batch).toBeDefined();
      expect(mockAdminFirestore.mockBatch).toHaveBeenCalled();
    });
  });

  describe("operations.createTimestamp", () => {
    it("Date から Timestamp を作成する", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );
      const date = new Date("2024-01-01T00:00:00Z");

      const timestamp = adapter.operations.createTimestamp(date);

      expect(timestamp).toBeDefined();
      expect(timestamp.toDate()).toEqual(date);
    });
  });

  describe("withConverter によるタイムスタンプ変換", () => {
    it("toFirestore の出力に含まれる Client SDK Timestamp が Admin SDK Timestamp に変換される", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const date = new Date("2024-06-15T12:00:00Z");
      const domainObject = {
        title: "test memo",
        timeline: {
          createdAt: date,
          updatedAt: date,
        },
      };

      let capturedData: unknown;
      const innerDocRef = {
        ...createMockDocumentRef(),
        set: vi.fn().mockImplementation((data: unknown) => {
          capturedData = data;
          return Promise.resolve(undefined);
        }),
      };

      const innerCollectionRef = {
        ...createMockCollectionRef(),
        withConverter: vi.fn().mockImplementation((converter: { toFirestore: (data: unknown) => unknown }) => {
          return {
            ...innerDocRef,
            doc: vi.fn().mockReturnValue({
              ...innerDocRef,
              set: vi.fn().mockImplementation((data: unknown, options?: unknown) => {
                const firestoreData = converter.toFirestore(data);
                capturedData = firestoreData;
                return Promise.resolve(undefined);
              }),
            }),
          };
        }),
      };

      mockAdminFirestore.mockCollection.mockReturnValue(innerCollectionRef);

      const converter = {
        toFirestore: (memo: typeof domainObject) => ({
          title: memo.title,
          timeline: {
            createdAt: ClientTimestamp.fromDate(memo.timeline.createdAt),
            updatedAt: ClientTimestamp.fromDate(memo.timeline.updatedAt),
          },
        }),
        fromFirestore: (snapshot: { data: () => unknown }) => snapshot.data(),
      };

      const collection = adapter.operations.collection(adapter.instance, "memos");
      const collectionWithConverter = (collection as { withConverter: (converter: unknown) => unknown }).withConverter(converter);
      const docRef = (collectionWithConverter as { doc: (path: string) => unknown }).doc("memo-1");
      const setMethod = (docRef as { set: (data: unknown) => Promise<unknown> }).set;

      setMethod(domainObject);

      expect(capturedData).toBeDefined();
      const captured = capturedData as {
        title: string;
        timeline: { createdAt: unknown; updatedAt: unknown };
      };
      expect(captured.title).toBe("test memo");
      expect(captured.timeline.createdAt).toBeInstanceOf(AdminTimestamp);
      expect(captured.timeline.updatedAt).toBeInstanceOf(AdminTimestamp);
      expect((captured.timeline.createdAt as AdminTimestamp).toDate()).toEqual(date);
    });

    it("Date オブジェクトがコンバーター到達前に破壊されない", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const date = new Date("2024-06-15T12:00:00Z");
      let converterReceivedData: unknown;

      const innerDocRef = createMockDocumentRef();
      const innerCollectionRef = {
        ...createMockCollectionRef(),
        withConverter: vi.fn().mockImplementation((converter: { toFirestore: (data: unknown) => unknown }) => {
          return {
            ...innerDocRef,
            doc: vi.fn().mockReturnValue({
              ...innerDocRef,
              set: vi.fn().mockImplementation((data: unknown) => {
                converterReceivedData = data;
                converter.toFirestore(data);
                return Promise.resolve(undefined);
              }),
            }),
          };
        }),
      };

      mockAdminFirestore.mockCollection.mockReturnValue(innerCollectionRef);

      const converter = {
        toFirestore: (data: { timeline: { createdAt: Date } }) => {
          expect(data.timeline.createdAt).toBeInstanceOf(Date);
          expect(data.timeline.createdAt.getTime()).toBe(date.getTime());
          return { createdAt: ClientTimestamp.fromDate(data.timeline.createdAt) };
        },
        fromFirestore: (snapshot: { data: () => unknown }) => snapshot.data(),
      };

      const collection = adapter.operations.collection(adapter.instance, "memos");
      const collectionWithConverter = (collection as { withConverter: (converter: unknown) => unknown }).withConverter(converter);
      const docRef = (collectionWithConverter as { doc: (path: string) => unknown }).doc("memo-1");
      (docRef as { set: (data: unknown) => Promise<unknown> }).set({
        timeline: { createdAt: date },
      });

      expect(converterReceivedData).toBeDefined();
      const received = converterReceivedData as { timeline: { createdAt: unknown } };
      expect(received.timeline.createdAt).toBeInstanceOf(Date);
    });

    it("トランザクション内で withConverter 経由の set がタイムスタンプを正しく変換する", async () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const date = new Date("2024-06-15T12:00:00Z");
      let capturedFirestoreData: unknown;
      let wrappedConverterReference: { toFirestore: (...args: unknown[]) => unknown } | undefined;

      const innerDocRef = createMockDocumentRef();

      const innerCollectionRef = {
        ...createMockCollectionRef(),
        withConverter: vi.fn().mockImplementation((wrappedConverter: { toFirestore: (...args: unknown[]) => unknown }) => {
          wrappedConverterReference = wrappedConverter;
          return {
            ...innerDocRef,
            doc: vi.fn().mockReturnValue(innerDocRef),
          };
        }),
      };

      mockAdminFirestore.mockCollection.mockReturnValue(innerCollectionRef);

      const mockTransaction: MockTransaction = {
        get: vi.fn().mockResolvedValue(createMockSnapshot(false)),
        set: vi.fn().mockImplementation((_ref: unknown, data: unknown) => {
          if (wrappedConverterReference) {
            capturedFirestoreData = wrappedConverterReference.toFirestore(data);
          } else {
            capturedFirestoreData = data;
          }
          return mockTransaction;
        }),
        update: vi.fn().mockReturnThis(),
        delete: vi.fn().mockReturnThis(),
      };

      mockAdminFirestore.mockRunTransaction.mockImplementation(
        async (callback: (transaction: MockTransaction) => Promise<unknown>) => {
          return callback(mockTransaction);
        },
      );

      const converter = {
        toFirestore: (memo: { title: string; createdAt: Date }) => ({
          title: memo.title,
          createdAt: ClientTimestamp.fromDate(memo.createdAt),
        }),
        fromFirestore: (snapshot: { data: () => unknown }) => snapshot.data(),
      };

      const collection = adapter.operations.collection(adapter.instance, "memos");
      const collectionWithConverter = (collection as { withConverter: (converter: unknown) => unknown }).withConverter(converter);

      await adapter.operations.runTransaction(
        adapter.instance,
        async (transaction) => {
          const docRef = (collectionWithConverter as { doc: (path: string) => unknown }).doc("memo-1");
          transaction.set(
            docRef as { get: () => Promise<unknown>; set: (data: unknown, options?: unknown) => unknown; update: (data: unknown) => unknown; delete: () => unknown; collection: (path: string) => unknown; withConverter: (converter: unknown) => unknown; id: string; path: string },
            { title: "test", createdAt: date },
          );
        },
      );

      expect(capturedFirestoreData).toBeDefined();
      const captured = capturedFirestoreData as {
        title: string;
        createdAt: unknown;
      };
      expect(captured.title).toBe("test");
      expect(captured.createdAt).toBeInstanceOf(AdminTimestamp);
      expect((captured.createdAt as AdminTimestamp).toDate()).toEqual(date);
    });

    it("配列内の entries の Timestamp も変換される", () => {
      const adapter = createAdminFirestoreAdapter(
        mockAdminFirestore.firestore,
      );

      const date1 = new Date("2024-01-01T00:00:00Z");
      const date2 = new Date("2024-06-15T12:00:00Z");

      let capturedData: unknown;

      const innerDocRef = createMockDocumentRef();
      const innerCollectionRef = {
        ...createMockCollectionRef(),
        withConverter: vi.fn().mockImplementation((converter: { toFirestore: (data: unknown) => unknown }) => ({
          ...innerDocRef,
          doc: vi.fn().mockReturnValue({
            ...innerDocRef,
            set: vi.fn().mockImplementation((data: unknown) => {
              capturedData = converter.toFirestore(data);
              return Promise.resolve(undefined);
            }),
          }),
        })),
      };

      mockAdminFirestore.mockCollection.mockReturnValue(innerCollectionRef);

      const converter = {
        toFirestore: (memo: { entries: Array<{ text: string; createdAt: Date }> }) => ({
          entries: memo.entries.map((entry) => ({
            text: entry.text,
            createdAt: ClientTimestamp.fromDate(entry.createdAt),
          })),
        }),
        fromFirestore: (snapshot: { data: () => unknown }) => snapshot.data(),
      };

      const collection = adapter.operations.collection(adapter.instance, "memos");
      const collectionWithConverter = (collection as { withConverter: (converter: unknown) => unknown }).withConverter(converter);
      const docRef = (collectionWithConverter as { doc: (path: string) => unknown }).doc("memo-1");
      (docRef as { set: (data: unknown) => Promise<unknown> }).set({
        entries: [
          { text: "entry 1", createdAt: date1 },
          { text: "entry 2", createdAt: date2 },
        ],
      });

      expect(capturedData).toBeDefined();
      const captured = capturedData as {
        entries: Array<{ text: string; createdAt: unknown }>;
      };
      expect(captured.entries).toHaveLength(2);
      expect(captured.entries[0].createdAt).toBeInstanceOf(AdminTimestamp);
      expect(captured.entries[1].createdAt).toBeInstanceOf(AdminTimestamp);
      expect((captured.entries[0].createdAt as AdminTimestamp).toDate()).toEqual(date1);
      expect((captured.entries[1].createdAt as AdminTimestamp).toDate()).toEqual(date2);
    });
  });
});
