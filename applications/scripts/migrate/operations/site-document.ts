import { Firestore } from "firebase-admin/firestore";
import { siteDocument } from "../data/privacy-policy";

const COLLECTION = "site-documents";
const DOCUMENT_ID = "site-document";

type Options = {
  dryRun: boolean;
};

export async function migrateSiteDocument(
  database: Firestore,
  options: Options,
): Promise<void> {
  console.log("\n--- Migrating Site Document ---");

  const reference = database.collection(COLLECTION).doc(DOCUMENT_ID);
  const snapshot = await reference.get();

  if (snapshot.exists) {
    console.log(`  Skipping: ${COLLECTION}/${DOCUMENT_ID} already exists`);
    return;
  }

  if (options.dryRun) {
    console.log(
      `  [DRY RUN] Would create ${COLLECTION}/${DOCUMENT_ID}`,
    );
  } else {
    await reference.set(siteDocument);
    console.log(`  Created ${COLLECTION}/${DOCUMENT_ID}`);
  }

  console.log("Site document migration complete.");
}
