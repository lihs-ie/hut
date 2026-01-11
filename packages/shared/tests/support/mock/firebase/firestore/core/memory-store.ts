
import { DocumentData } from "../../types/firestore"

export class MemoryStore {
  private readonly documents = new Map<string, DocumentData>()

  setDocument(path: string, data: DocumentData): void {
    this.documents.set(path, { ...data })
  }

  getDocument(path: string): DocumentData | undefined {
    const data = this.documents.get(path)
    return data ? { ...data } : undefined
  }

  hasDocument(path: string): boolean {
    return this.documents.has(path)
  }

  deleteDocument(path: string): void {
    this.documents.delete(path)
  }

  updateDocument(path: string, data: Partial<DocumentData>): void {
    const existing = this.documents.get(path)
    if (existing) {
      this.documents.set(path, { ...existing, ...data })
    }
  }

  getCollectionDocuments(
    collectionPath: string
  ): Array<{ id: string; data: DocumentData; path: string }> {
    const documents: Array<{ id: string; data: DocumentData; path: string }> =
      []

    for (const [path, data] of this.documents.entries()) {
      if (this.isDocumentInCollection(path, collectionPath)) {
        const id = this.getDocumentIdFromPath(path)
        documents.push({ id, data: { ...data }, path })
      }
    }

    return documents
  }

  private isDocumentInCollection(
    documentPath: string,
    collectionPath: string
  ): boolean {
    const pathSegments = documentPath.split("/")
    const collectionSegments = collectionPath.split("/")

    if (collectionSegments.length % 2 === 0) return false

    if (pathSegments.length % 2 !== 0) return false

    if (pathSegments.length !== collectionSegments.length + 1) return false

    for (let i = 0; i < collectionSegments.length; i++) {
      if (pathSegments[i] !== collectionSegments[i]) return false
    }

    return true
  }

  private getDocumentIdFromPath(path: string): string {
    const segments = path.split("/")
    return segments[segments.length - 1] || ""
  }

  getAllDocumentPaths(): string[] {
    return Array.from(this.documents.keys())
  }

  clear(): void {
    this.documents.clear()
  }
}
