import { Firestore, Timestamp } from "firebase-admin/firestore";
import { BATCH_SIZE } from "../common";

type Options = {
  dryRun: boolean;
};

type CollectionConfig = {
  name: string;
  toPublishedAt: (
    timeline: { createdAt: unknown },
  ) => string | Timestamp | null;
};

const toISOString = (timeline: { createdAt: unknown }): string | null => {
  const createdAt = timeline.createdAt;
  if (typeof createdAt === "string") {
    return createdAt;
  }
  if (createdAt instanceof Date) {
    return createdAt.toISOString();
  }
  return null;
};

const toTimestamp = (timeline: { createdAt: unknown }): Timestamp | null => {
  const createdAt = timeline.createdAt;
  if (createdAt instanceof Timestamp) {
    return createdAt;
  }
  if (createdAt instanceof Date) {
    return Timestamp.fromDate(createdAt);
  }
  return null;
};

const COLLECTIONS: CollectionConfig[] = [
  { name: "articles", toPublishedAt: toISOString },
  { name: "memos", toPublishedAt: toTimestamp },
  { name: "series", toPublishedAt: toTimestamp },
  { name: "chapters", toPublishedAt: toTimestamp },
];

async function patchCollection(
  database: Firestore,
  config: CollectionConfig,
  options: Options,
): Promise<{ updated: number; skipped: number; total: number }> {
  console.log(`\n  Processing collection: ${config.name}`);

  const snapshot = await database.collection(config.name).get();
  let batch = database.batch();
  let batchCount = 0;
  let updated = 0;
  let skipped = 0;

  for (const document of snapshot.docs) {
    const data = document.data();

    if (data.publishedAt !== undefined) {
      console.log(`    Skip (already has publishedAt): ${document.id}`);
      skipped++;
      continue;
    }

    const isPublished = data.status === "published";
    const publishedAt = isPublished
      ? config.toPublishedAt(data.timeline)
      : null;

    console.log(
      `    ${options.dryRun ? "[DRY RUN] " : ""}Update: ${document.id} → publishedAt=${publishedAt}`,
    );

    if (!options.dryRun) {
      batch.update(document.ref, { publishedAt });
    }

    batchCount++;
    updated++;

    if (batchCount >= BATCH_SIZE) {
      if (!options.dryRun) {
        await batch.commit();
        console.log(`    Committed batch of ${batchCount} operations`);
      }
      batch = database.batch();
      batchCount = 0;
    }
  }

  if (batchCount > 0) {
    if (!options.dryRun) {
      await batch.commit();
      console.log(`    Committed final batch of ${batchCount} operations`);
    }
  }

  return { updated, skipped, total: snapshot.size };
}

export async function migratePublishedAt(
  database: Firestore,
  options: Options,
): Promise<void> {
  console.log("\n--- Migrating publishedAt ---");

  const results: Record<
    string,
    { updated: number; skipped: number; total: number }
  > = {};

  for (const config of COLLECTIONS) {
    results[config.name] = await patchCollection(database, config, options);
  }

  console.log("\n  Summary:");
  for (const [collection, result] of Object.entries(results)) {
    console.log(
      `    ${collection}: ${result.updated} updated, ${result.skipped} skipped (total: ${result.total})`,
    );
  }
}
