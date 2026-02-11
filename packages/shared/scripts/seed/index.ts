import {
  BASE_URL,
  FIRESTORE_EMULATOR_HOST,
  PROJECT_ID,
  DATABASE_ID,
  clearCollection,
  TAG_IDS,
  ARTICLE_IDS,
  SERIES_IDS,
  MEMO_IDS,
  ADMIN_ID,
} from "./common";
import { seedTags } from "./tag";
import { seedArticles } from "./article";
import { seedSeries } from "./series";
import { seedMemos } from "./memo";
import { seedAdmin } from "./admin";
import { seedSearchTokens } from "./search-token";
import { seedSiteDocument } from "./site-document";
import { seedAnalytics } from "./analytics";

async function clearAllData(): Promise<void> {
  console.log("\n=== Clearing existing data ===");

  const emulatorClearUrl = `${FIRESTORE_EMULATOR_HOST}/emulator/v1/projects/${PROJECT_ID}/databases/${DATABASE_ID}/documents`;
  try {
    const response = await fetch(emulatorClearUrl, { method: "DELETE" });
    if (response.ok) {
      console.log("Cleared all Firestore emulator data");
      return;
    }
  } catch {
  }

  await clearCollection("tags");
  await clearCollection("articles");
  await clearCollection("series");
  await clearCollection("memos");
  await clearCollection("admin");
  await clearCollection("search-tokens");
  await clearCollection("site-documents");
  await clearCollection("index/tags/name");
  await clearCollection("index/articles/slug");
  await clearCollection("index/memos/slug");
  await clearCollection("index/series/slug");
  await clearCollection("page-view-counters");
  await clearCollection("page-view-dedup");
  await clearCollection("access-logs");
  await clearCollection("unique-visitor-counters");
  await clearCollection("unique-visitor-dedup");
  await clearCollection("engagement-logs");
  await clearCollection("search-logs");
  await clearCollection("rate-limits");
}

async function main(): Promise<void> {
  console.log("=== Starting seed script ===");
  console.log(`Target: ${BASE_URL}`);

  try {
    await clearAllData();

    await seedTags();
    await seedArticles();
    await seedSeries();
    await seedMemos();
    await seedAdmin();
    await seedSearchTokens();
    await seedSiteDocument();
    await seedAnalytics();

    console.log("\n=== Seed completed successfully ===");
    console.log("\nCreated IDs:");
    console.log("Tags:", TAG_IDS);
    console.log("Articles:", ARTICLE_IDS);
    console.log("Series:", SERIES_IDS);
    console.log("Memos:", MEMO_IDS);
    console.log("Admin:", ADMIN_ID);
    console.log("SiteDocument:", "site-document");
    console.log("Analytics:", "seeded");
  } catch (error) {
    console.error("Seed failed:", error);
    process.exit(1);
  }
}

main();
