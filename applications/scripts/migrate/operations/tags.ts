import { Firestore, FieldValue } from "firebase-admin/firestore";
import { BATCH_SIZE } from "../common";
import { tags } from "../data/tags";

type Options = {
  dryRun: boolean;
};

export async function migrateTags(
  database: Firestore,
  options: Options,
): Promise<void> {
  console.log("\n--- Migrating Tags ---");

  const now = new Date().toISOString();
  let batch = database.batch();
  let batchCount = 0;
  let createdCount = 0;
  let skippedCount = 0;

  for (const tag of tags) {
    const tagReference = database.collection("tags").doc(tag.identifier);
    const tagSnapshot = await tagReference.get();

    if (tagSnapshot.exists) {
      console.log(`  Skipping existing tag: ${tag.name} (${tag.identifier})`);
      skippedCount++;
      continue;
    }

    if (!options.dryRun) {
      batch.set(tagReference, {
        identifier: tag.identifier,
        name: tag.name,
        logo: tag.logo,
        timeline: {
          createdAt: now,
          updatedAt: now,
        },
        version: 1,
      });
    }

    const indexReference = database
      .collection("index")
      .doc("tags")
      .collection("name")
      .doc(tag.name);

    const indexSnapshot = await indexReference.get();

    if (!indexSnapshot.exists) {
      if (!options.dryRun) {
        batch.set(indexReference, {
          referenceIdentifier: tag.identifier,
          createdAt: FieldValue.serverTimestamp(),
        });
      }
    }

    batchCount += 2;
    createdCount++;

    if (batchCount >= BATCH_SIZE) {
      if (!options.dryRun) {
        await batch.commit();
        console.log(`  Committed batch of ${batchCount} operations`);
      } else {
        console.log(
          `  [DRY RUN] Would commit batch of ${batchCount} operations`,
        );
      }
      batch = database.batch();
      batchCount = 0;
    }
  }

  if (batchCount > 0) {
    if (!options.dryRun) {
      await batch.commit();
      console.log(`  Committed final batch of ${batchCount} operations`);
    } else {
      console.log(
        `  [DRY RUN] Would commit final batch of ${batchCount} operations`,
      );
    }
  }

  console.log(
    `Tags migration complete: ${createdCount} created, ${skippedCount} skipped (total: ${tags.length})`,
  );
}
