/**
 * Firestore Emulator テストデータ作成スクリプト
 *
 * 使用方法:
 *   pnpm seed
 *
 * 前提条件:
 *   - Firebase Emulatorが起動していること (port 8085)
 */

import {
  BASE_URL,
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

async function clearAllData(): Promise<void> {
  console.log("\n=== Clearing existing data ===");
  await clearCollection("tags");
  await clearCollection("articles");
  await clearCollection("series");
  await clearCollection("memos");
  await clearCollection("admin");
  await clearCollection("search-tokens");
  await clearCollection("site-documents");
  // インデックスコレクションのクリア
  await clearCollection("index/tags/name");
  await clearCollection("index/articles/slug");
  await clearCollection("index/memos/slug");
  await clearCollection("index/series/slug");
}

async function main(): Promise<void> {
  console.log("=== Starting seed script ===");
  console.log(`Target: ${BASE_URL}`);

  try {
    // 既存データをクリア
    await clearAllData();

    // データを作成
    await seedTags();
    await seedArticles();
    await seedSeries();
    await seedMemos();
    await seedAdmin();
    await seedSearchTokens();
    await seedSiteDocument();

    console.log("\n=== Seed completed successfully ===");
    console.log("\nCreated IDs:");
    console.log("Tags:", TAG_IDS);
    console.log("Articles:", ARTICLE_IDS);
    console.log("Series:", SERIES_IDS);
    console.log("Memos:", MEMO_IDS);
    console.log("Admin:", ADMIN_ID);
    console.log("SiteDocument:", "site-document");
  } catch (error) {
    console.error("Seed failed:", error);
    process.exit(1);
  }
}

main();
