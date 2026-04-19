import type { Firestore } from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { FirebaseProvider } from "@shared/providers/infrastructure/firebase";
import { FirebaseRestProvider } from "@shared/providers/infrastructure/firebase-rest";

const isCloudflareBuild = process.env.BUILD_TARGET === "cloudflare";

export const ReaderFirestoreProvider = {
  get instance(): Firestore {
    return isCloudflareBuild
      ? FirebaseRestProvider.firestore.instance
      : FirebaseProvider.firestore.instance;
  },
  get operations(): FirestoreOperations {
    return isCloudflareBuild
      ? FirebaseRestProvider.firestore.operations
      : FirebaseProvider.firestore.operations;
  },
} as const;
