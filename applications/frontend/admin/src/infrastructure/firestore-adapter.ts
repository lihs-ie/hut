import {
  Timestamp as AdminTimestamp,
} from "firebase-admin/firestore";
import type { Firestore } from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";

export type AdminFirestoreCompatible = {
  collection: (path: string) => unknown;
  doc: (path: string) => unknown;
  collectionGroup: (collectionId: string) => unknown;
  runTransaction: <T>(updateFunction: (transaction: unknown) => Promise<T>) => Promise<T>;
  batch: () => unknown;
};

type ConstraintDescriptor = {
  _type: string;
  _args: unknown[];
};

const isConstraintDescriptor = (value: unknown): value is ConstraintDescriptor =>
  value !== null &&
  typeof value === "object" &&
  "_type" in value &&
  "_args" in value;

const createConstraint = (type: string, ...args: unknown[]): ConstraintDescriptor => ({
  _type: type,
  _args: args,
});

type QueryLike = {
  where: (...args: unknown[]) => QueryLike;
  orderBy: (...args: unknown[]) => QueryLike;
  limit: (n: number) => QueryLike;
  limitToLast: (n: number) => QueryLike;
  startAt: (...args: unknown[]) => QueryLike;
  startAfter: (...args: unknown[]) => QueryLike;
  endAt: (...args: unknown[]) => QueryLike;
  endBefore: (...args: unknown[]) => QueryLike;
};

const isQueryLike = (value: unknown): value is QueryLike =>
  value !== null &&
  typeof value === "object" &&
  "where" in value &&
  typeof (value as Record<string, unknown>).where === "function";

const applyConstraint = (query: QueryLike, constraint: ConstraintDescriptor): QueryLike => {
  switch (constraint._type) {
    case "where":
      return query.where(constraint._args[0], constraint._args[1], constraint._args[2]);
    case "orderBy":
      return query.orderBy(constraint._args[0], constraint._args[1]);
    case "limit":
      return query.limit(Number(constraint._args[0]));
    case "limitToLast":
      return query.limitToLast(Number(constraint._args[0]));
    case "startAt":
      return query.startAt(...constraint._args);
    case "startAfter":
      return query.startAfter(...constraint._args);
    case "endAt":
      return query.endAt(...constraint._args);
    case "endBefore":
      return query.endBefore(...constraint._args);
    case "and":
    case "or":
      return constraint._args.reduce<QueryLike>(
        (q, c) => isConstraintDescriptor(c) ? applyConstraint(q, c) : q,
        query,
      );
    default:
      return query;
  }
};

type SnapshotLike = {
  exists: boolean;
  data: () => unknown;
  id: string;
  ref: unknown;
  get: (fieldPath: string) => unknown;
};

const wrapSnapshot = (snapshot: SnapshotLike) => {
  const existsValue = snapshot.exists;
  return new Proxy(snapshot, {
    get(target, prop) {
      if (prop === "exists") {
        return () => existsValue;
      }
      return Reflect.get(target, prop);
    },
  });
};

type QuerySnapshotLike = {
  docs: SnapshotLike[];
  size: number;
  empty: boolean;
};

const wrapQuerySnapshot = (querySnapshot: QuerySnapshotLike) => {
  const wrappedDocs = querySnapshot.docs.map(wrapSnapshot);
  return new Proxy(querySnapshot, {
    get(target, prop) {
      if (prop === "docs") {
        return wrappedDocs;
      }
      if (prop === "forEach") {
        return (callback: (doc: unknown) => void) => {
          wrappedDocs.forEach(callback);
        };
      }
      return Reflect.get(target, prop);
    },
  });
};

type DocumentRefLike = {
  get: () => Promise<SnapshotLike>;
  set: (data: unknown, options?: unknown) => Promise<unknown>;
  update: (data: unknown) => Promise<unknown>;
  delete: () => Promise<unknown>;
  collection: (path: string) => CollectionRefLike;
  withConverter: (converter: unknown) => DocumentRefLike;
  id: string;
  path: string;
};

type CollectionRefLike = {
  doc: (path?: string) => DocumentRefLike;
  add: (data: unknown) => Promise<DocumentRefLike>;
  withConverter: (converter: unknown) => CollectionRefLike;
  get: () => Promise<QuerySnapshotLike>;
  where: (...args: unknown[]) => QueryLike;
  orderBy: (...args: unknown[]) => QueryLike;
};

const hasMethod = <M extends string>(
  value: unknown,
  methodName: M,
): value is Record<M, (...args: unknown[]) => unknown> =>
  value !== null &&
  typeof value === "object" &&
  methodName in value &&
  typeof (value as Record<string, unknown>)[methodName] === "function";

type TransactionLike = {
  get: (ref: DocumentRefLike) => Promise<SnapshotLike>;
  set: (ref: DocumentRefLike, data: unknown, options?: unknown) => TransactionLike;
  update: (ref: DocumentRefLike, data: unknown) => TransactionLike;
  delete: (ref: DocumentRefLike) => TransactionLike;
};

const isTransactionLike = (value: unknown): value is TransactionLike =>
  hasMethod(value, "get") &&
  hasMethod(value, "set") &&
  hasMethod(value, "update") &&
  hasMethod(value, "delete");

const wrapTransaction = (transaction: TransactionLike): TransactionLike => {
  return new Proxy(transaction, {
    get(target, prop) {
      if (prop === "get") {
        return async (ref: DocumentRefLike) => wrapSnapshot(await target.get(ref));
      }
      return Reflect.get(target, prop);
    },
  });
};

export const createAdminFirestoreAdapter = (
  adminFirestore: AdminFirestoreCompatible,
): { instance: Firestore; operations: FirestoreOperations } => {
  const buildFullPath = (path: string, ...pathSegments: string[]): string =>
    [path, ...pathSegments].join("/");

  const operations = {
    collection: (_firestore: unknown, path: string, ...pathSegments: string[]) =>
      adminFirestore.collection(buildFullPath(path, ...pathSegments)),

    doc: (reference: unknown, path?: string, ...pathSegments: string[]) => {
      if (!path) {
        return reference;
      }
      const fullPath = buildFullPath(path, ...pathSegments);
      if (hasMethod(reference, "doc")) {
        return reference.doc(fullPath);
      }
      return adminFirestore.doc(fullPath);
    },

    getDoc: async (reference: unknown) => {
      if (!hasMethod(reference, "get")) {
        throw new Error("Reference does not have a get method");
      }
      const snapshot = await reference.get();
      return wrapSnapshot(snapshot as SnapshotLike);
    },

    getDocs: async (queryRef: unknown) => {
      if (!hasMethod(queryRef, "get")) {
        throw new Error("Query reference does not have a get method");
      }
      const snapshot = await queryRef.get();
      return wrapQuerySnapshot(snapshot as QuerySnapshotLike);
    },

    setDoc: async (reference: unknown, data: unknown, options?: unknown) => {
      if (!hasMethod(reference, "set")) {
        throw new Error("Reference does not have a set method");
      }
      if (options) {
        await reference.set(data, options);
      } else {
        await reference.set(data);
      }
    },

    updateDoc: async (reference: unknown, data: unknown) => {
      if (!hasMethod(reference, "update")) {
        throw new Error("Reference does not have an update method");
      }
      await reference.update(data);
    },

    deleteDoc: async (reference: unknown) => {
      if (!hasMethod(reference, "delete")) {
        throw new Error("Reference does not have a delete method");
      }
      await reference.delete();
    },

    addDoc: async (reference: unknown, data: unknown) => {
      if (!hasMethod(reference, "add")) {
        throw new Error("Reference does not have an add method");
      }
      return reference.add(data);
    },

    query: (queryRef: unknown, ...constraints: ConstraintDescriptor[]) => {
      if (!isQueryLike(queryRef)) {
        return queryRef;
      }
      return constraints.reduce<unknown>(
        (q, constraint) => isQueryLike(q) ? applyConstraint(q, constraint) : q,
        queryRef,
      );
    },

    where: (fieldPath: string, opStr: string, value: unknown) =>
      createConstraint("where", fieldPath, opStr, value),

    orderBy: (fieldPath: string, directionStr?: string) =>
      createConstraint("orderBy", fieldPath, directionStr),

    limit: (n: number) => createConstraint("limit", n),

    limitToLast: (n: number) => createConstraint("limitToLast", n),

    startAt: (...values: unknown[]) => createConstraint("startAt", ...values),

    startAfter: (...values: unknown[]) => createConstraint("startAfter", ...values),

    endAt: (...values: unknown[]) => createConstraint("endAt", ...values),

    endBefore: (...values: unknown[]) => createConstraint("endBefore", ...values),

    and: (...queryConstraints: ConstraintDescriptor[]) =>
      createConstraint("and", ...queryConstraints),

    or: (...queryConstraints: ConstraintDescriptor[]) =>
      createConstraint("or", ...queryConstraints),

    collectionGroup: (_firestore: unknown, collectionId: string) =>
      adminFirestore.collectionGroup(collectionId),

    runTransaction: async <T>(
      _firestore: unknown,
      updateFunction: (transaction: TransactionLike) => Promise<T>,
    ): Promise<T> => {
      return adminFirestore.runTransaction(async (transaction) => {
        if (!isTransactionLike(transaction)) {
          throw new Error("Admin SDK transaction does not satisfy TransactionLike interface");
        }
        return updateFunction(wrapTransaction(transaction));
      });
    },

    writeBatch: (_firestore: unknown) => adminFirestore.batch(),

    createTimestamp: (date: Date) => AdminTimestamp.fromDate(date),
  };

  // @ts-expect-error Admin SDK objects are runtime-compatible with Client SDK interfaces
  return { instance: adminFirestore, operations };
};
