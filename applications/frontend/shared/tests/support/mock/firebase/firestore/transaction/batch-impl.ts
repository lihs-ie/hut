
import {
  DocumentData,
  DocumentReference,
  FieldPath,
  SetOptions,
  UpdateData,
  WriteBatch
} from "../../types/firestore"
import { FirestoreImpl } from "../database/firestore-impl"
import { DocumentReferenceImpl } from "../reference/reference-impl"
import { CommonOperation, OperationExecutor } from "./operation-utils"

export class WriteBatchImpl implements WriteBatch {
  private operations: CommonOperation[] = []

  constructor(private firestore: FirestoreImpl) {}

  set<T = DocumentData>(documentRef: DocumentReference<T>, data: T): WriteBatch
  set<T = DocumentData>(
    documentRef: DocumentReference<T>,
    data: Partial<T>,
    options: SetOptions
  ): WriteBatch
  set<T = DocumentData>(
    documentRef: DocumentReference<T>,
    data: T | Partial<T>,
    options?: SetOptions
  ): WriteBatch {
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
  ): WriteBatch
  update<T = DocumentData>(
    documentRef: DocumentReference<T>,
    field: string | FieldPath,
    value: unknown,
    ...moreFieldsAndValues: unknown[]
  ): WriteBatch
  update<T = DocumentData>(
    documentRef: DocumentReference<T>,
    dataOrField: UpdateData<T> | string | FieldPath,
    value?: unknown,
    ...moreFieldsAndValues: unknown[]
  ): WriteBatch {
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

  delete(documentRef: DocumentReference): WriteBatch {
    this.operations.push({
      type: "delete",
      ref: documentRef as DocumentReferenceImpl
    })
    return this
  }

  async commit(): Promise<void> {
    const memoryStore = this.firestore._getMemoryStore()

    for (const operation of this.operations) {
      OperationExecutor.applyOperation(memoryStore, operation)
    }

    this.operations = []
  }
}
