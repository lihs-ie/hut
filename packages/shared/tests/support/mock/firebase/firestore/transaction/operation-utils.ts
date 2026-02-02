
import { MemoryStore } from "../core/memory-store"
import { SetOptions } from "../../types/firestore"
import { DocumentReferenceImpl } from "../reference/reference-impl"

export type CommonOperation =
  | {
      type: "set"
      ref: DocumentReferenceImpl
      data: Record<string, unknown>
      options?: SetOptions | undefined
    }
  | {
      type: "update"
      ref: DocumentReferenceImpl
      data: Record<string, unknown>
    }
  | {
      type: "delete"
      ref: DocumentReferenceImpl
    }

export class OperationExecutor {
  static applyOperation(memoryStore: MemoryStore, operation: CommonOperation): void {
    switch (operation.type) {
      case "set":
        this.applySetOperation(memoryStore, operation)
        break

      case "update":
        this.applyUpdateOperation(memoryStore, operation)
        break

      case "delete":
        memoryStore.deleteDocument(operation.ref.path)
        break
    }
  }

  private static applySetOperation(
    memoryStore: MemoryStore,
    operation: CommonOperation & { type: "set" }
  ): void {
    if (operation.options?.merge || operation.options?.mergeFields) {
      const existingData = memoryStore.getDocument(operation.ref.path) || {}
      if (operation.options.mergeFields) {
        const mergedData = { ...existingData }
        for (const field of operation.options.mergeFields) {
          if (field in operation.data) {
            mergedData[field] = operation.data[field]
          }
        }
        memoryStore.setDocument(operation.ref.path, mergedData)
      } else {
        memoryStore.setDocument(operation.ref.path, {
          ...existingData,
          ...operation.data
        })
      }
    } else {
      memoryStore.setDocument(operation.ref.path, operation.data)
    }
  }

  private static applyUpdateOperation(
    memoryStore: MemoryStore,
    operation: CommonOperation & { type: "update" }
  ): void {
    const existingData = memoryStore.getDocument(operation.ref.path)
    if (!existingData) {
      throw new Error(`Document ${operation.ref.path} does not exist`)
    }
    const updatedData = { ...existingData }
    for (const [key, value] of Object.entries(operation.data)) {
      if (key.includes(".")) {
        this.setNestedValue(updatedData, key, value)
      } else {
        updatedData[key] = value
      }
    }
    memoryStore.setDocument(operation.ref.path, updatedData)
  }

  private static setNestedValue(obj: Record<string, unknown>, path: string, value: unknown): void {
    const keys = path.split(".")
    let current: Record<string, unknown> = obj

    for (let i = 0; i < keys.length - 1; i++) {
      const key = keys[i]
      if (!key) continue
      if (
        !(key in current) ||
        typeof current[key] !== "object" ||
        current[key] === null
      ) {
        current[key] = {}
      }
      current = current[key] as Record<string, unknown>
    }

    const lastKey = keys[keys.length - 1]
    if (lastKey) {
      current[lastKey] = value
    }
  }
}
