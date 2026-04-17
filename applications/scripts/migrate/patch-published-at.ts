import { initializeFirestore } from "./common";
import { migratePublishedAt } from "./operations/published-at";

async function main(): Promise<void> {
  const projectId = process.env.FIREBASE_PROJECT_ID;
  const dryRun = process.argv.includes("--dry-run");

  if (!projectId) {
    console.error("FIREBASE_PROJECT_ID environment variable is required.");
    console.error(
      "Usage: FIREBASE_PROJECT_ID=<project-id> npx tsx migrate/patch-published-at.ts [--dry-run]",
    );
    process.exit(1);
  }

  if (dryRun) {
    console.log("=== DRY RUN MODE - No changes will be made ===\n");
  }

  console.log(
    `Starting publishedAt patch for project: ${projectId}\n`,
  );

  try {
    const database = initializeFirestore(projectId);

    await migratePublishedAt(database, { dryRun });

    console.log("\n=== Patch Complete ===");
    if (dryRun) {
      console.log(
        "[DRY RUN] No changes were made. Run without --dry-run to apply changes.",
      );
    } else {
      console.log("Patch applied successfully!");
    }
  } catch (error) {
    console.error("\nPatch failed:", error);
    process.exit(1);
  }
}

main();
