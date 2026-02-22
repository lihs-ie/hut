import { Firestore } from "firebase-admin/firestore";
import { adminProfile } from "../data/admin-profile";

const COLLECTION = "admin";
const DOCUMENT_ID = "admin";

type Options = {
  dryRun: boolean;
};

export async function migrateAdminProfile(
  database: Firestore,
  options: Options,
): Promise<void> {
  console.log("\n--- Migrating Admin Profile ---");

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
    await reference.set(adminProfile);
    console.log(`  Created ${COLLECTION}/${DOCUMENT_ID}`);
  }

  console.log("Admin profile migration complete.");
}
