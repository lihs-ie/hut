import { initializeApp, cert, type ServiceAccount } from "firebase-admin/app";
import { getFirestore, FieldValue } from "firebase-admin/firestore";
import * as path from "path";
import * as fs from "fs";

const getServiceAccount = (): ServiceAccount => {
  const serviceAccountPath =
    process.env.GOOGLE_APPLICATION_CREDENTIALS ||
    path.join(__dirname, "../service-account-key.json");

  if (!fs.existsSync(serviceAccountPath)) {
    throw new Error(
      `Service account file not found: ${serviceAccountPath}\n` +
        "Please set GOOGLE_APPLICATION_CREDENTIALS environment variable or place service-account-key.json in the project root.",
    );
  }

  return JSON.parse(
    fs.readFileSync(serviceAccountPath, "utf-8"),
  ) as ServiceAccount;
};

type CollectionConfig = {
  name: "articles" | "memos" | "series";
  slugField: string;
  identifierField: string;
};

const collections: CollectionConfig[] = [
  { name: "articles", slugField: "slug", identifierField: "identifier" },
  { name: "memos", slugField: "slug", identifierField: "identifier" },
  { name: "series", slugField: "slug", identifierField: "identifier" },
];

type MigrationStats = {
  collection: string;
  total: number;
  created: number;
  duplicates: string[];
};

async function checkDuplicates(
  database: FirebaseFirestore.Firestore,
  config: CollectionConfig,
): Promise<Map<string, string[]>> {
  console.log(`Checking duplicates in ${config.name}...`);

  const snapshot = await database.collection(config.name).get();
  const slugMap = new Map<string, string[]>();

  for (const document of snapshot.docs) {
    const data = document.data();
    const slug = data[config.slugField] as string;
    const identifier = data[config.identifierField] as string;

    if (!slugMap.has(slug)) {
      slugMap.set(slug, []);
    }
    slugMap.get(slug)!.push(identifier);
  }

  return slugMap;
}

async function migrateCollection(
  database: FirebaseFirestore.Firestore,
  config: CollectionConfig,
  dryRun: boolean,
): Promise<MigrationStats> {
  console.log(`\nMigrating ${config.name}...`);

  const slugMap = await checkDuplicates(database, config);
  const duplicates: string[] = [];

  for (const [slug, identifiers] of slugMap.entries()) {
    if (identifiers.length > 1) {
      duplicates.push(`"${slug}" is used by: ${identifiers.join(", ")}`);
    }
  }

  if (duplicates.length > 0) {
    console.error(`\nDUPLICATE SLUGS FOUND in ${config.name}:`);
    duplicates.forEach((duplicate) => console.error(`  - ${duplicate}`));
    throw new Error(
      `${duplicates.length} duplicate slug(s) found in ${config.name}. Please resolve manually before migration.`,
    );
  }

  const snapshot = await database.collection(config.name).get();
  const batchSize = 500;
  let batch = database.batch();
  let batchCount = 0;
  let createdCount = 0;

  for (const document of snapshot.docs) {
    const data = document.data();
    const slug = data[config.slugField] as string;
    const identifier = data[config.identifierField] as string;

    const indexPath = `index/${config.name}/slug/${slug}`;
    const indexReference = database.doc(indexPath);

    const existingIndex = await indexReference.get();
    if (existingIndex.exists) {
      console.log(`  Skipping existing index: ${indexPath}`);
      continue;
    }

    if (!dryRun) {
      batch.set(indexReference, {
        referenceIdentifier: identifier,
        collectionName: config.name,
        createdAt: FieldValue.serverTimestamp(),
      });
    }

    batchCount++;
    createdCount++;

    if (batchCount >= batchSize) {
      if (!dryRun) {
        await batch.commit();
        console.log(`  Committed batch of ${batchCount} indexes`);
      } else {
        console.log(`  [DRY RUN] Would commit batch of ${batchCount} indexes`);
      }
      batch = database.batch();
      batchCount = 0;
    }
  }

  if (batchCount > 0) {
    if (!dryRun) {
      await batch.commit();
      console.log(`  Committed final batch of ${batchCount} indexes`);
    } else {
      console.log(
        `  [DRY RUN] Would commit final batch of ${batchCount} indexes`,
      );
    }
  }

  console.log(
    `Completed ${config.name}: ${createdCount} indexes ${dryRun ? "would be " : ""}created`,
  );

  return {
    collection: config.name,
    total: snapshot.size,
    created: createdCount,
    duplicates,
  };
}

async function main(): Promise<void> {
  const dryRun = process.argv.includes("--dry-run");

  if (dryRun) {
    console.log("=== DRY RUN MODE - No changes will be made ===\n");
  }

  console.log("Starting slug index migration...\n");

  try {
    const serviceAccount = getServiceAccount();
    const app = initializeApp({
      credential: cert(serviceAccount),
    });

    const database = getFirestore(app);
    const stats: MigrationStats[] = [];

    for (const config of collections) {
      const result = await migrateCollection(database, config, dryRun);
      stats.push(result);
    }

    console.log("\n=== Migration Summary ===");
    for (const stat of stats) {
      console.log(
        `${stat.collection}: ${stat.created}/${stat.total} indexes ${dryRun ? "would be " : ""}created`,
      );
    }

    if (dryRun) {
      console.log(
        "\n[DRY RUN] No changes were made. Run without --dry-run to apply changes.",
      );
    } else {
      console.log("\nMigration completed successfully!");
    }
  } catch (error) {
    console.error("\nMigration failed:", error);
    process.exit(1);
  }
}

main();
