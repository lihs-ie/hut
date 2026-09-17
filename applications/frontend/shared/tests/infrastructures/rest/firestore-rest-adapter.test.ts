import { describe, it, expect } from "vitest";
import {
  initializeApp as initClientApp,
  getApps as getClientApps,
  deleteApp,
} from "firebase/app";
import { getFirestore, Timestamp } from "firebase/firestore";
import { createRestFirestoreAdapter } from "@shared/infrastructures/rest/firestore-rest-adapter";

describe("infrastructures/rest/firestore-rest-adapter", () => {
  it("operations に全 FirestoreOperations メソッドが含まれる", async () => {
    const app = initClientApp(
      { projectId: "demo-hut", apiKey: "fake-api-key" },
      `test-${Date.now()}`,
    );
    try {
      const firestore = getFirestore(app);
      const { instance, operations } = createRestFirestoreAdapter(firestore);

      expect(instance).toBe(firestore);

      expect(typeof operations.doc).toBe("function");
      expect(typeof operations.collection).toBe("function");
      expect(typeof operations.getDoc).toBe("function");
      expect(typeof operations.getDocs).toBe("function");
      expect(typeof operations.setDoc).toBe("function");
      expect(typeof operations.updateDoc).toBe("function");
      expect(typeof operations.deleteDoc).toBe("function");
      expect(typeof operations.addDoc).toBe("function");
      expect(typeof operations.query).toBe("function");
      expect(typeof operations.where).toBe("function");
      expect(typeof operations.orderBy).toBe("function");
      expect(typeof operations.limit).toBe("function");
      expect(typeof operations.limitToLast).toBe("function");
      expect(typeof operations.startAt).toBe("function");
      expect(typeof operations.startAfter).toBe("function");
      expect(typeof operations.endAt).toBe("function");
      expect(typeof operations.endBefore).toBe("function");
      expect(typeof operations.and).toBe("function");
      expect(typeof operations.or).toBe("function");
      expect(typeof operations.collectionGroup).toBe("function");
      expect(typeof operations.runTransaction).toBe("function");
      expect(typeof operations.writeBatch).toBe("function");
      expect(typeof operations.createTimestamp).toBe("function");
    } finally {
      const cleanupApp = getClientApps().find((a) => a.name === app.name);
      if (cleanupApp) {
        await deleteApp(cleanupApp);
      }
    }
  });

  it("createTimestamp が Client SDK の Timestamp を返す", async () => {
    const app = initClientApp(
      { projectId: "demo-hut", apiKey: "fake-api-key" },
      `test-${Date.now()}-ts`,
    );
    try {
      const firestore = getFirestore(app);
      const { operations } = createRestFirestoreAdapter(firestore);

      const date = new Date("2026-01-01T00:00:00Z");
      const timestamp = operations.createTimestamp(date);

      expect(timestamp).toBeInstanceOf(Timestamp);
      expect(timestamp.toDate().getTime()).toBe(date.getTime());
    } finally {
      const cleanupApp = getClientApps().find((a) => a.name === app.name);
      if (cleanupApp) {
        await deleteApp(cleanupApp);
      }
    }
  });
});
