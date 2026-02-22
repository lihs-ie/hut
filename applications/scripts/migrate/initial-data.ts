import { initializeFirestore } from "./common";
import { migrateTags } from "./operations/tags";
import { migrateAdminProfile } from "./operations/admin-profile";
import { migrateSiteDocument } from "./operations/site-document";

async function main(): Promise<void> {
  const projectId = process.env.FIREBASE_PROJECT_ID;
  const dryRun = process.argv.includes("--dry-run");

  if (!projectId) {
    console.error("FIREBASE_PROJECT_ID environment variable is required.");
    process.exit(1);
  }

  if (dryRun) {
    console.log("=== DRY RUN MODE - No changes will be made ===\n");
  }

  console.log(`Starting initial data migration for project: ${projectId}\n`);

  try {
    const database = initializeFirestore(projectId);

    await migrateTags(database, { dryRun });
    await migrateAdminProfile(database, { dryRun });
    await migrateSiteDocument(database, { dryRun });

    console.log("\n=== Migration Summary ===");
    if (dryRun) {
      console.log(
        "[DRY RUN] No changes were made. Run without --dry-run to apply changes.",
      );
    } else {
      console.log("Migration completed successfully!");
    }
  } catch (error) {
    console.error("\nMigration failed:", error);
    process.exit(1);
  }
}

main();
