import type { Firestore } from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { FirebaseAdminProvider } from "../auth/admin";
import { createAdminFirestoreAdapter } from "../../infrastructure/firestore";

let cachedAdapter: { instance: Firestore; operations: FirestoreOperations } | null = null;

const getAdapter = (): { instance: Firestore; operations: FirestoreOperations } => {
  if (cachedAdapter === null) {
    cachedAdapter = createAdminFirestoreAdapter(
      FirebaseAdminProvider.firestore.instance,
    );
  }
  return cachedAdapter;
};

export const AdminFirestoreProvider = {
  firestore: {
    get instance(): Firestore {
      return getAdapter().instance;
    },
    get operations(): FirestoreOperations {
      return getAdapter().operations;
    },
  },
} as const;
