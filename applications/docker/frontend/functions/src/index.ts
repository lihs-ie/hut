import { onRequest } from "firebase-functions/v2/https";
import { onDocumentCreated } from "firebase-functions/v2/firestore";
import { onCustomEventPublished } from "firebase-functions/v2/eventarc";
import * as logger from "firebase-functions/logger";

// HTTP関数のサンプル
export const helloWorld = onRequest((request, response) => {
  logger.info("Hello logs!", { structuredData: true });
  response.send("Hello from Firebase!");
});

// Firestoreトリガーのサンプル
export const onArticleCreated = onDocumentCreated(
  "articles/{articleId}",
  (event) => {
    const snapshot = event.data;
    if (!snapshot) {
      logger.warn("No data associated with the event");
      return;
    }
    const data = snapshot.data();
    logger.info("Article created:", data);
  }
);

// Eventarcカスタムイベントハンドラのサンプル
export const onCustomEvent = onCustomEventPublished(
  "my-custom-event",
  (event) => {
    logger.info("Custom event received:", event.data);
  }
);
