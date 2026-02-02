
import {
  DocumentData,
  DocumentReference,
  FieldPath,
  SetOptions,
  Transaction,
  UpdateData
} from "../../types/firestore"
import { FirestoreImpl } from "../database/firestore-impl"
import { DocumentReferenceImpl } from "../reference/reference-impl"
import { DocumentSnapshotImpl } from "../reference/snapshot-impl"
import { CommonOperation, OperationExecutor } from "./operation-utils"

export class TransactionImpl implements Transaction {
  private operations: CommonOperation[] = []

  constructor(private firestore: FirestoreImpl) {}

  async get<T = DocumentData>(
    documentRef: DocumentReference<T>
  ): Promise<DocumentSnapshotImpl<T>> {
    const memoryStore = this.firestore._getMemoryStore()
    const data = memoryStore.getDocument(documentRef.path)

    return new DocumentSnapshotImpl(
      documentRef as DocumentReferenceImpl<T>,
      data as T
    )
  }

  set<T = DocumentData>(documentRef: DocumentReference<T>, data: T): Transaction
  set<T = DocumentData>(
    documentRef: DocumentReference<T>,
    data: Partial<T>,
    options: SetOptions
  ): Transaction
  set<T = DocumentData>(
    documentRef: DocumentReference<T>,
    data: T | Partial<T>,
    options?: SetOptions
  ): Transaction {
    this.operations.push({
      type: "set",
      ref: documentRef as DocumentReferenceImpl,
      data,
      options
    })
    return this
  }

  update<T = DocumentData>(
    documentRef: DocumentReference<T>,
    data: UpdateData<T>
  ): Transaction
  update<T = DocumentData>(
    documentRef: DocumentReference<T>,
    field: string | FieldPath,
    value: unknown,
    ...moreFieldsAndValues: unknown[]
  ): Transaction
  update<T = DocumentData>(
    documentRef: DocumentReference<T>,
    dataOrField: UpdateData<T> | string | FieldPath,
    value?: unknown,
    ...moreFieldsAndValues: unknown[]
  ): Transaction {
    let updateData: Record<string, unknown>

    if (
      typeof dataOrField === "object" &&
      !(dataOrField instanceof FieldPath)
    ) {
      updateData = dataOrField as Record<string, unknown>
    } else {
      updateData = {}
      const fieldPath =
        dataOrField instanceof FieldPath ? dataOrField.toString() : dataOrField
      updateData[fieldPath] = value

      for (let i = 0; i < moreFieldsAndValues.length; i += 2) {
        const field = moreFieldsAndValues[i]
        const val = moreFieldsAndValues[i + 1]
        const path = field instanceof FieldPath ? field.toString() : String(field)
        updateData[path] = val
      }
    }

    this.operations.push({
      type: "update",
      ref: documentRef as DocumentReferenceImpl,
      data: updateData
    })
    return this
  }

  delete(documentRef: DocumentReference): Transaction {
    this.operations.push({
      type: "delete",
      ref: documentRef as DocumentReferenceImpl
    })
    return this
  }

  async _commit(): Promise<void> {
    const memoryStore = this.firestore._getMemoryStore()

    for (const operation of this.operations) {
      OperationExecutor.applyOperation(memoryStore, operation)
    }
  }
}
