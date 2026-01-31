import { FirebaseApp } from "firebase/app";

export interface DocumentData {
  [field: string]: any;
}

export interface FirestoreSettings {
  ignoreUndefinedProperties?: boolean;
  host?: string;
  ssl?: boolean;
  cacheSizeBytes?: number;
}

export interface Firestore {
  readonly app: FirebaseApp;
  readonly type: "firestore" | "firestore-lite";
  toJSON(): object;
}

export interface DocumentReference<T = DocumentData> {
  readonly id: string;
  readonly path: string;
  readonly type: "document";
  readonly firestore: Firestore;
  readonly converter: FirestoreDataConverter<T> | null;
  readonly parent: CollectionReference<T>;
}

export interface CollectionReference<T = DocumentData> {
  readonly id: string;
  readonly path: string;
  readonly type: "collection";
  readonly firestore: Firestore;
  readonly converter: FirestoreDataConverter<T> | null;
  readonly parent: DocumentReference | null;
}

export interface DocumentSnapshot<T = DocumentData> {
  readonly id: string;
  readonly ref: DocumentReference<T>;
  readonly metadata: SnapshotMetadata;
  exists(): boolean;
  data(): T | undefined;
  get(fieldPath: string): any;
  toJSON(): object;
}

export interface QueryDocumentSnapshot<T = DocumentData>
  extends DocumentSnapshot<T> {
  data(): T;
}

export interface QuerySnapshot<T = DocumentData> {
  readonly docs: QueryDocumentSnapshot<T>[];
  readonly empty: boolean;
  readonly size: number;
  readonly metadata: SnapshotMetadata;
  readonly query: Query<T>;
  forEach(
    callback: (result: QueryDocumentSnapshot<T>) => void,
    thisArg?: any
  ): void;
  toJSON(): object;
}

export interface Query<T = DocumentData> {
  readonly firestore: Firestore;
  readonly converter: FirestoreDataConverter<T> | null;
  readonly type: "query" | "collection";
}

export abstract class QueryConstraint {
  abstract readonly type: QueryConstraintType;
}

export type QueryConstraintType =
  | "where"
  | "orderBy"
  | "limit"
  | "limitToLast"
  | "startAt"
  | "startAfter"
  | "endAt"
  | "endBefore"
  | "and"
  | "or";

export class QueryFieldFilterConstraint extends QueryConstraint {
  readonly type = "where" as const;
}

export class QueryCompositeFilterConstraint extends QueryConstraint {
  readonly type: "or" | "and";
  constructor(type: "or" | "and") {
    super();
    this.type = type;
  }
}

export class QueryOrderByConstraint extends QueryConstraint {
  readonly type = "orderBy" as const;
}

export class QueryLimitConstraint extends QueryConstraint {
  readonly type: "limit" | "limitToLast";
  constructor(type: "limit" | "limitToLast") {
    super();
    this.type = type;
  }
}

export class QueryStartAtConstraint extends QueryConstraint {
  readonly type: "startAt" | "startAfter";
  constructor(type: "startAt" | "startAfter") {
    super();
    this.type = type;
  }
}

export class QueryEndAtConstraint extends QueryConstraint {
  readonly type: "endBefore" | "endAt";
  constructor(type: "endBefore" | "endAt") {
    super();
    this.type = type;
  }
}

export type QueryFilterConstraint =
  | QueryFieldFilterConstraint
  | QueryCompositeFilterConstraint;

export type QueryNonFilterConstraint =
  | QueryOrderByConstraint
  | QueryLimitConstraint
  | QueryStartAtConstraint
  | QueryEndAtConstraint;

export type WhereFilterOp =
  | "<"
  | "<="
  | "=="
  | "!="
  | ">="
  | ">"
  | "array-contains"
  | "in"
  | "array-contains-any"
  | "not-in";

export type OrderByDirection = "desc" | "asc";

export class FieldPath {
  constructor(...fieldNames: string[]) {
    this.fieldNames = fieldNames;
  }

  private readonly fieldNames: string[];

  isEqual(other: FieldPath): boolean {
    return (
      this.fieldNames.length === other.fieldNames.length &&
      this.fieldNames.every((name, index) => name === other.fieldNames[index])
    );
  }

  toString(): string {
    return this.fieldNames.join(".");
  }
}

export interface SnapshotMetadata {
  readonly fromCache: boolean;
  readonly hasPendingWrites: boolean;
  isEqual(other: SnapshotMetadata): boolean;
}

export interface FirestoreDataConverter<T, DbModelType = DocumentData> {
  toFirestore(modelObject: T): DbModelType;
  toFirestore(
    modelObject: Partial<T>,
    options: SetOptions
  ): Partial<DbModelType>;
  fromFirestore(snapshot: QueryDocumentSnapshot<DbModelType>): T;
}

export interface SetOptions {
  readonly merge?: boolean;
  readonly mergeFields?: string[];
}

export type UpdateData<T> = T extends Primitive
  ? T
  : T extends {}
  ? {
      [K in keyof T]?: UpdateData<T[K]> | FieldValue;
    } & NestedUpdateFields<T>
  : Partial<T>;

export interface NestedUpdateFields<T extends Record<string, unknown>> {}

export type Primitive = string | number | boolean | undefined | null;

export class FieldValue {
  isEqual(other: FieldValue): boolean {
    return this === other;
  }
}

export type TransactionFunction<T> = (transaction: Transaction) => Promise<T>;

export interface Transaction {
  get<T = DocumentData>(
    documentRef: DocumentReference<T>
  ): Promise<DocumentSnapshot<T>>;
  set<T = DocumentData>(
    documentRef: DocumentReference<T>,
    data: T
  ): Transaction;
  set<T = DocumentData>(
    documentRef: DocumentReference<T>,
    data: Partial<T>,
    options: SetOptions
  ): Transaction;
  update<T = DocumentData>(
    documentRef: DocumentReference<T>,
    data: UpdateData<T>
  ): Transaction;
  update<T = DocumentData>(
    documentRef: DocumentReference<T>,
    field: string | FieldPath,
    value: any,
    ...moreFieldsAndValues: any[]
  ): Transaction;
  delete(documentRef: DocumentReference<any>): Transaction;
}

export interface WriteBatch {
  set<T = DocumentData>(documentRef: DocumentReference<T>, data: T): WriteBatch;
  set<T = DocumentData>(
    documentRef: DocumentReference<T>,
    data: Partial<T>,
    options: SetOptions
  ): WriteBatch;
  update<T = DocumentData>(
    documentRef: DocumentReference<T>,
    data: UpdateData<T>
  ): WriteBatch;
  update<T = DocumentData>(
    documentRef: DocumentReference<T>,
    field: string | FieldPath,
    value: any,
    ...moreFieldsAndValues: any[]
  ): WriteBatch;
  delete(documentRef: DocumentReference<any>): WriteBatch;
  commit(): Promise<void>;
}

export interface FirestoreTimestamp {
  toDate(): Date;
  seconds: number;
  nanoseconds: number;
}

export interface FirestoreOperations {
  doc<T = DocumentData>(
    firestore: Firestore,
    path: string,
    ...pathSegments: string[]
  ): DocumentReference<T>;
  doc<T = DocumentData>(
    reference: CollectionReference<T>,
    path?: string,
    ...pathSegments: string[]
  ): DocumentReference<T>;
  doc<T = DocumentData>(
    reference: DocumentReference<T>,
    path: string,
    ...pathSegments: string[]
  ): DocumentReference<T>;

  collection<T = DocumentData>(
    firestore: Firestore,
    path: string,
    ...pathSegments: string[]
  ): CollectionReference<T>;

  getDoc<T = DocumentData>(
    reference: DocumentReference<T>
  ): Promise<DocumentSnapshot<T>>;
  getDocs<T = DocumentData>(query: Query<T>): Promise<QuerySnapshot<T>>;
  setDoc<T = DocumentData>(
    reference: DocumentReference<T>,
    data: T,
    options?: SetOptions
  ): Promise<void>;
  updateDoc<T = DocumentData>(
    reference: DocumentReference<T>,
    data: UpdateData<T>
  ): Promise<void>;
  deleteDoc(reference: DocumentReference<DocumentData>): Promise<void>;
  addDoc<T = DocumentData>(
    reference: CollectionReference<T>,
    data: T
  ): Promise<DocumentReference<T>>;
  query<T = DocumentData>(
    query: Query<T>,
    ...constraints: QueryConstraint[]
  ): Query<T>;
  where(
    fieldPath: string,
    opStr: WhereFilterOp,
    value: unknown
  ): QueryConstraint;
  orderBy(fieldPath: string, directionStr?: OrderByDirection): QueryConstraint;
  limit(limit: number): QueryConstraint;
  limitToLast(limit: number): QueryConstraint;
  startAt(...values: unknown[]): QueryConstraint;
  startAfter(...values: unknown[]): QueryConstraint;
  endAt(...values: unknown[]): QueryConstraint;
  endBefore(...values: unknown[]): QueryConstraint;
  and(...queryConstraints: QueryFilterConstraint[]): QueryConstraint;
  or(...queryConstraints: QueryFilterConstraint[]): QueryConstraint;
  collectionGroup(
    firestore: Firestore,
    collectionId: string
  ): Query<DocumentData>;

  runTransaction<T>(
    firestore: Firestore,
    updateFunction: TransactionFunction<T>
  ): Promise<T>;
  writeBatch(firestore: Firestore): WriteBatch;

  createTimestamp(date: Date): FirestoreTimestamp;
}
