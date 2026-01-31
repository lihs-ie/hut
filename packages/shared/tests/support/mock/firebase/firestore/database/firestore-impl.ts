
import { FirebaseApp } from "firebase/app"

import { Firestore, FirestoreSettings } from "../../types/firestore"
import { MemoryStore } from "../core/memory-store"

export class FirestoreImpl implements Firestore {
  readonly type = "firestore" as const
  private readonly memoryStore: MemoryStore

  constructor(
    readonly app: FirebaseApp,
    private readonly settings?: FirestoreSettings
  ) {
    this.memoryStore = new MemoryStore()
  }

  _getMemoryStore(): MemoryStore {
    return this.memoryStore
  }

  toJSON(): object {
    return {
      app: this.app.name,
      settings: this.settings
    }
  }
}

let defaultFirestoreInstance: FirestoreImpl | null = null

export function getFirestore(): FirestoreImpl {
  if (!defaultFirestoreInstance) {
    const mockApp = { name: "[DEFAULT]" } as FirebaseApp
    defaultFirestoreInstance = new FirestoreImpl(mockApp)
  }
  return defaultFirestoreInstance
}

export function initializeFirestore(
  app: FirebaseApp,
  settings: FirestoreSettings
): FirestoreImpl {
  return new FirestoreImpl(app, settings)
}

export function _clearDefaultInstance(): void {
  if (defaultFirestoreInstance) {
    defaultFirestoreInstance._getMemoryStore().clear()
  }
  defaultFirestoreInstance = null
}
