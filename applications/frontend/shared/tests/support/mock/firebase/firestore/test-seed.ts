
import type { DocumentData, Firestore } from "../types/firestore"
import { FirestoreImpl } from "./database/firestore-impl"

export interface FirestoreFixture {
  [collectionPath: string]: {
    [documentId: string]: DocumentData
  }
}

export async function seedFirestore(
  firestore: Firestore,
  fixture: FirestoreFixture
): Promise<void> {
  const firestoreImpl = firestore as FirestoreImpl
  const memoryStore = firestoreImpl._getMemoryStore()

  for (const [collectionPath, documents] of Object.entries(fixture)) {
    for (const [documentId, data] of Object.entries(documents)) {
      const documentPath = `${collectionPath}/${documentId}`
      memoryStore.setDocument(documentPath, data)
    }
  }
}

export async function createTestFirestoreWithSeed(fixture: FirestoreFixture) {
  const { createTestFirestoreOperations } = await import("./test-operations")
  const { firestore, operations } = createTestFirestoreOperations()

  await seedFirestore(firestore, fixture)

  return { firestore, operations }
}

export async function seedCollection(
  firestore: Firestore,
  collectionPath: string,
  documents: DocumentData[]
): Promise<string[]> {
  const firestoreImpl = firestore as FirestoreImpl
  const memoryStore = firestoreImpl._getMemoryStore()

  const documentIds: string[] = []

  for (let i = 0; i < documents.length; i++) {
    const documentId = `doc_${Date.now()}_${i}`
    const documentPath = `${collectionPath}/${documentId}`
    const documentData = documents[i]
    if (documentData) {
      memoryStore.setDocument(documentPath, documentData)
      documentIds.push(documentId)
    }
  }

  return documentIds
}

export async function loadFixture(
  firestore: Firestore,
  fixtureData: {
    collections: {
      [collectionName: string]: Array<{
        id: string
        data: DocumentData
      }>
    }
  }
): Promise<void> {
  const fixture: FirestoreFixture = {}

  for (const [collectionName, documents] of Object.entries(
    fixtureData.collections
  )) {
    fixture[collectionName] = {}
    for (const { id, data } of documents) {
      fixture[collectionName][id] = data
    }
  }

  await seedFirestore(firestore, fixture)
}

export function clearFirestore(firestore: Firestore): void {
  const firestoreImpl = firestore as FirestoreImpl
  const memoryStore = firestoreImpl._getMemoryStore()
  memoryStore.clear()
}

function generateAutoId(): string {
  const chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  let result = ""
  for (let i = 0; i < 20; i++) {
    result += chars.charAt(Math.floor(Math.random() * chars.length))
  }
  return result
}

export async function addDocsWithAutoId(
  firestore: Firestore,
  collectionPath: string,
  documents: DocumentData[]
): Promise<string[]> {
  const firestoreImpl = firestore as FirestoreImpl
  const memoryStore = firestoreImpl._getMemoryStore()

  const documentIds: string[] = []

  for (const document of documents) {
    const documentId = generateAutoId()
    const documentPath = `${collectionPath}/${documentId}`
    memoryStore.setDocument(documentPath, document)
    documentIds.push(documentId)
  }

  return documentIds
}

export async function addDocWithAutoId(
  firestore: Firestore,
  collectionPath: string,
  data: DocumentData
): Promise<string> {
  const documentIds = await addDocsWithAutoId(firestore, collectionPath, [data])
  const firstId = documentIds[0]
  if (!firstId) {
    throw new Error("Failed to create document with auto ID")
  }
  return firstId
}

export async function seedFirestoreWithAutoIds(
  firestore: Firestore,
  fixture: {
    [collectionPath: string]: {
      [documentIdOrAuto: string]: DocumentData | DocumentData[]
    }
  }
): Promise<{ [collectionPath: string]: string[] }> {
  const firestoreImpl = firestore as FirestoreImpl
  const memoryStore = firestoreImpl._getMemoryStore()
  const createdIds: { [collectionPath: string]: string[] } = {}

  for (const [collectionPath, documents] of Object.entries(fixture)) {
    createdIds[collectionPath] = []

    for (const [documentIdOrAuto, data] of Object.entries(documents)) {
      if (documentIdOrAuto === "__auto__" && Array.isArray(data)) {
        const autoIds = await addDocsWithAutoId(firestore, collectionPath, data)
        createdIds[collectionPath].push(...autoIds)
      } else if (documentIdOrAuto === "__auto__" && !Array.isArray(data)) {
        const autoId = await addDocWithAutoId(firestore, collectionPath, data)
        createdIds[collectionPath].push(autoId)
      } else {
        const documentPath = `${collectionPath}/${documentIdOrAuto}`
        memoryStore.setDocument(documentPath, data as DocumentData)
        createdIds[collectionPath].push(documentIdOrAuto)
      }
    }
  }

  return createdIds
}
