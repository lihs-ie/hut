import type { Firestore } from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { FirebaseAdminProvider } from "./firebase-admin";
import { createAdminFirestoreAdapter } from "@shared/infrastructures/firestore-admin-adapter";
import { FirebaseAdminStorageImageUploader } from "@shared/infrastructures/storage-admin";
import type { ImageUploader } from "@shared/domains/common/image";

let cachedAdapter: { instance: Firestore; operations: FirestoreOperations } | null = null;

const getAdapter = (): { instance: Firestore; operations: FirestoreOperations } => {
  if (cachedAdapter === null) {
    cachedAdapter = createAdminFirestoreAdapter(
      FirebaseAdminProvider.firestore.instance,
    );
  }
  return cachedAdapter;
};

let cachedImageUploader: ImageUploader | null = null;

const getImageUploader = (): ImageUploader => {
  if (cachedImageUploader === null) {
    cachedImageUploader = FirebaseAdminStorageImageUploader(
      FirebaseAdminProvider.storage.instance,
    );
  }
  return cachedImageUploader;
};

export const FirebaseProvider = {
  firestore: {
    get instance(): Firestore {
      return getAdapter().instance;
    },
    get operations(): FirestoreOperations {
      return getAdapter().operations;
    },
  },
  storage: {
    get imageUploader(): ImageUploader {
      return getImageUploader();
    },
  },
} as const;
