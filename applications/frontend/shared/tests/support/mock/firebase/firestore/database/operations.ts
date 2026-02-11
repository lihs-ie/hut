
import { DocumentData, SetOptions, UpdateData } from "../../types/firestore"
import { createInvalidArgumentError, createNotFoundError } from "../core/errors"
import {
  CollectionReferenceImpl,
  DocumentReferenceImpl
} from "../reference/reference-impl"
import {
  DocumentSnapshotImpl,
  QuerySnapshotImpl
} from "../reference/snapshot-impl"
import { FirestoreImpl } from "./firestore-impl"

export async function addDoc<T = DocumentData>(
  reference: CollectionReferenceImpl<T>,
  data: T
): Promise<DocumentReferenceImpl<T>> {
  const docRef = new DocumentReferenceImpl<T>(
    reference.firestore,
    `${reference.path}/${generateAutoId()}`,
    reference.converter // CollectionReferenceのconverterを継承
  )
  await setDoc(docRef, data)
  return docRef
}

export async function setDoc<T = DocumentData>(
  reference: DocumentReferenceImpl<T>,
  data: T,
  options?: SetOptions
): Promise<void> {
  const firestore = reference.firestore as FirestoreImpl
  const memoryStore = firestore._getMemoryStore()

  let processedData: DocumentData
  if (reference.converter) {
    try {
      if (options?.merge || options?.mergeFields) {
        processedData = reference.converter.toFirestore(
          data as Partial<T>,
          options
        )
      } else {
        processedData = reference.converter.toFirestore(data)
      }
    } catch (error) {
      console.warn("toFirestore conversion failed:", error)
      processedData = data as DocumentData
    }
  } else {
    processedData = data as DocumentData
  }

  if (options?.merge) {
    const existingData = memoryStore.getDocument(reference.path)
    const mergedData = existingData
      ? { ...existingData, ...processedData }
      : processedData
    memoryStore.setDocument(reference.path, mergedData)
  } else {
    memoryStore.setDocument(reference.path, processedData)
  }
}

export async function getDoc<T = DocumentData>(
  reference: DocumentReferenceImpl<T>
): Promise<DocumentSnapshotImpl<T>> {
  const firestore = reference.firestore as FirestoreImpl
  const memoryStore = firestore._getMemoryStore()

  const data = memoryStore.getDocument(reference.path)
  return new DocumentSnapshotImpl(reference, data as T | undefined)
}

export async function updateDoc<T = DocumentData>(
  reference: DocumentReferenceImpl<T>,
  data: UpdateData<T>
): Promise<void> {
  if (!data || typeof data !== "object") {
    throw createInvalidArgumentError("Update data must be an object")
  }

  const firestore = reference.firestore as FirestoreImpl
  const memoryStore = firestore._getMemoryStore()

  if (!memoryStore.hasDocument(reference.path)) {
    throw createNotFoundError(reference.path)
  }

  let processedData: Partial<DocumentData>
  if (reference.converter) {
    try {
      processedData = reference.converter.toFirestore(data as Partial<T>, {
        merge: true
      })
    } catch (error) {
      console.warn("toFirestore conversion failed:", error)
      processedData = data as Partial<DocumentData>
    }
  } else {
    processedData = data as Partial<DocumentData>
  }

  memoryStore.updateDocument(reference.path, processedData)
}

export async function deleteDoc<T = DocumentData>(
  reference: DocumentReferenceImpl<T>
): Promise<void> {
  const firestore = reference.firestore as FirestoreImpl
  const memoryStore = firestore._getMemoryStore()

  memoryStore.deleteDocument(reference.path)
}

export async function getDocs<T = DocumentData>(
  query:
    | CollectionReferenceImpl<T>
    | { execute(): Promise<QuerySnapshotImpl<T>> }
): Promise<QuerySnapshotImpl<T>> {
  if ("execute" in query) {
    return query.execute()
  }

  const firestore = query.firestore as FirestoreImpl
  const memoryStore = firestore._getMemoryStore()

  const documents = memoryStore.getCollectionDocuments(query.path)
  const docSnapshots = documents.map(({ id: _id, data, path }) => ({
    ref: new DocumentReferenceImpl<T>(firestore, path, query.converter), // converterを引き継ぐ
    data: data as T
  }))

  return new QuerySnapshotImpl(query, docSnapshots)
}

function generateAutoId(): string {
  const chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  let result = ""
  for (let i = 0; i < 20; i++) {
    result += chars.charAt(Math.floor(Math.random() * chars.length))
  }
  return result
}
