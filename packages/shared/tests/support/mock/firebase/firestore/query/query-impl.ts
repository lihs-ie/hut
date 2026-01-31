
import {
  DocumentData,
  FirestoreDataConverter,
  Query,
  QueryConstraint
} from "../../types/firestore"
import { FirestoreImpl } from "../database/firestore-impl"
import {
  CollectionReferenceImpl,
  DocumentReferenceImpl
} from "../reference/reference-impl"
import { QuerySnapshotImpl } from "../reference/snapshot-impl"
import {
  QueryCompositeFilterConstraintImpl,
  QueryEndAtConstraintImpl,
  QueryFieldFilterConstraintImpl,
  QueryLimitConstraintImpl,
  QueryOrderByConstraintImpl,
  QueryStartAtConstraintImpl
} from "./constraints"
import { DocumentEntry, QueryEngine } from "./query-engine"

export class QueryImpl<T = DocumentData> implements Query<T> {
  readonly type = "query" as const

  constructor(
    readonly firestore: FirestoreImpl,
    private readonly basePath: string,
    private readonly constraints: QueryConstraint[] = [],
    readonly converter: FirestoreDataConverter<T> | null = null
  ) {}

  async execute(): Promise<QuerySnapshotImpl<T>> {
    const memoryStore = this.firestore._getMemoryStore()
    let documents: DocumentEntry[]

    if (this.basePath.startsWith("__collectionGroup__")) {
      const collectionId = this.basePath.replace("__collectionGroup__", "")
      documents = this.getCollectionGroupDocuments(memoryStore, collectionId)
    } else {
      documents = memoryStore.getCollectionDocuments(this.basePath)
    }

    const filterConstraints = this.constraints.filter(
      (c) =>
        c instanceof QueryFieldFilterConstraintImpl ||
        c instanceof QueryCompositeFilterConstraintImpl
    ) as Array<
      QueryFieldFilterConstraintImpl | QueryCompositeFilterConstraintImpl
    >

    const orderByConstraints = this.constraints.filter(
      (c) => c instanceof QueryOrderByConstraintImpl
    ) as QueryOrderByConstraintImpl[]

    const limitConstraint = this.constraints.find(
      (c) => c instanceof QueryLimitConstraintImpl
    ) as QueryLimitConstraintImpl | undefined

    const startAtConstraint = this.constraints.find(
      (c) => c instanceof QueryStartAtConstraintImpl
    ) as QueryStartAtConstraintImpl | undefined

    const endAtConstraint = this.constraints.find(
      (c) => c instanceof QueryEndAtConstraintImpl
    ) as QueryEndAtConstraintImpl | undefined

    let filteredDocs = QueryEngine.applyFilters(documents, filterConstraints)

    if (orderByConstraints.length > 0) {
      filteredDocs = QueryEngine.applyOrderBy(filteredDocs, orderByConstraints)
    }

    if (startAtConstraint) {
      filteredDocs = QueryEngine.applyStartAt(
        filteredDocs,
        startAtConstraint,
        orderByConstraints
      )
    }

    if (endAtConstraint) {
      filteredDocs = QueryEngine.applyEndAt(
        filteredDocs,
        endAtConstraint,
        orderByConstraints
      )
    }

    if (limitConstraint) {
      filteredDocs = QueryEngine.applyLimit(filteredDocs, limitConstraint)
    }

    const docSnapshots = filteredDocs.map(({ path, data }) => ({
      ref: new DocumentReferenceImpl<T>(this.firestore, path, this.converter),
      data: data as T
    }))

    return new QuerySnapshotImpl(this, docSnapshots)
  }

  private getCollectionGroupDocuments(
    memoryStore: { getAllDocumentPaths(): string[]; getDocument(path: string): DocumentData | undefined },
    collectionId: string
  ): DocumentEntry[] {
    const allPaths = memoryStore.getAllDocumentPaths()
    const groupDocs: DocumentEntry[] = []

    for (const path of allPaths) {
      const segments = path.split("/")
      if (
        segments.length >= 2 &&
        segments[segments.length - 2] === collectionId
      ) {
        const data = memoryStore.getDocument(path)
        if (data) {
          groupDocs.push({
            id: segments[segments.length - 1],
            path,
            data
          })
        }
      }
    }

    return groupDocs
  }
}

export function query<T = DocumentData>(
  queryOrRef: Query<T>,
  ...queryConstraints: QueryConstraint[]
): QueryImpl<T> {
  let firestore: FirestoreImpl
  let basePath: string
  let existingConstraints: QueryConstraint[] = []
  let converter: FirestoreDataConverter<T> | null = null

  if (queryOrRef instanceof QueryImpl) {
    firestore = queryOrRef.firestore
    basePath = queryOrRef["basePath"]
    existingConstraints = queryOrRef["constraints"]
    converter = queryOrRef.converter
  } else if (queryOrRef instanceof CollectionReferenceImpl) {
    firestore = queryOrRef.firestore as FirestoreImpl
    basePath = queryOrRef.path
    converter = queryOrRef.converter
  } else {
    throw new Error("Invalid query source")
  }

  return new QueryImpl(
    firestore,
    basePath,
    [...existingConstraints, ...queryConstraints],
    converter
  )
}

export function collectionGroup(
  firestore: FirestoreImpl,
  collectionId: string
): QueryImpl<DocumentData> {
  return new QueryImpl(firestore, `__collectionGroup__${collectionId}`)
}
