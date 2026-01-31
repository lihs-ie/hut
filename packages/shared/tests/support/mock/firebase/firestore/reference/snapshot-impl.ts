
import {
  DocumentData,
  DocumentSnapshot,
  Query,
  QueryDocumentSnapshot,
  QuerySnapshot,
  SnapshotMetadata
} from "../../types/firestore"
import { DocumentReferenceImpl } from "./reference-impl"

export class SnapshotMetadataImpl implements SnapshotMetadata {
  constructor(
    readonly fromCache: boolean = false,
    readonly hasPendingWrites: boolean = false
  ) {}

  isEqual(other: SnapshotMetadata): boolean {
    return (
      this.fromCache === other.fromCache &&
      this.hasPendingWrites === other.hasPendingWrites
    )
  }
}

export class DocumentSnapshotImpl<T = DocumentData>
  implements DocumentSnapshot<T>
{
  readonly metadata: SnapshotMetadata

  constructor(
    readonly ref: DocumentReferenceImpl<T>,
    protected readonly data_: T | undefined
  ) {
    this.metadata = new SnapshotMetadataImpl()
  }

  get id(): string {
    return this.ref.id
  }

  exists(): boolean {
    return this.data_ !== undefined
  }

  data(): T | undefined {
    if (!this.data_) return undefined

    if (this.ref.converter) {
      try {
        const refWithoutConverter = new DocumentReferenceImpl<T>(
          this.ref.firestore,
          this.ref.path,
          null // converterをnullに設定
        )

        const querySnapshot = new QueryDocumentSnapshotImpl<DocumentData>(
          refWithoutConverter as DocumentReferenceImpl<DocumentData>,
          this.data_ as DocumentData
        )
        return this.ref.converter.fromFirestore(querySnapshot) as T
      } catch (error) {
        console.warn("fromFirestore conversion failed:", error)
        return this.data_
      }
    }

    return this.data_
  }

  get(fieldPath: string): unknown {
    if (!this.data_) return undefined

    const data = this.data_ as Record<string, unknown>
    const pathSegments = fieldPath.split(".")

    let current: unknown = data
    for (const segment of pathSegments) {
      if (current === null || current === undefined) return undefined
      if (typeof current !== "object") return undefined
      current = (current as Record<string, unknown>)[segment]
    }

    return current
  }

  toJSON(): object {
    return {
      id: this.id,
      data: this.data_,
      exists: this.exists(),
      metadata: {
        fromCache: this.metadata.fromCache,
        hasPendingWrites: this.metadata.hasPendingWrites
      }
    }
  }
}

export class QueryDocumentSnapshotImpl<T = DocumentData>
  extends DocumentSnapshotImpl<T>
  implements QueryDocumentSnapshot<T>
{
  constructor(ref: DocumentReferenceImpl<T>, data: T) {
    super(ref, data)
  }

  override data(): T {
    if (!this.data_) {
      throw new Error("Document does not exist")
    }

    return super.data()!
  }
}

export class QuerySnapshotImpl<T = DocumentData> implements QuerySnapshot<T> {
  readonly metadata: SnapshotMetadata
  readonly docs: QueryDocumentSnapshot<T>[]

  constructor(
    readonly query: Query<T>,
    documents: Array<{ ref: DocumentReferenceImpl<T>; data: T }>
  ) {
    this.metadata = new SnapshotMetadataImpl()
    this.docs = documents.map(
      ({ ref, data }) => new QueryDocumentSnapshotImpl(ref, data)
    )
  }

  get empty(): boolean {
    return this.docs.length === 0
  }

  get size(): number {
    return this.docs.length
  }

  forEach(
    callback: (result: QueryDocumentSnapshot<T>) => void,
    thisArg?: unknown
  ): void {
    this.docs.forEach(callback, thisArg)
  }

  toJSON(): object {
    return {
      empty: this.empty,
      size: this.size,
      docs: this.docs.map((doc) => doc.toJSON()),
      metadata: {
        fromCache: this.metadata.fromCache,
        hasPendingWrites: this.metadata.hasPendingWrites
      }
    }
  }
}
