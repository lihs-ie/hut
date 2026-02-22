import { initializeApp, applicationDefault } from "firebase-admin/app";
import { getFirestore } from "firebase-admin/firestore";

export const initializeFirestore = (projectId: string) => {
  const app = initializeApp({
    credential: applicationDefault(),
    projectId,
  });
  return getFirestore(app);
};

export const BATCH_SIZE = 500;
