import { getCloudflareContext } from "@opennextjs/cloudflare";
import { unexpectedError } from "@shared/aspects/error";
import { fromPromise } from "@shared/aspects/result";
import { ArticleRepository, criteriaSchema } from "@shared/domains/articles";
import { FirebaseArticleRepository } from "@shared/infrastructures/articles";
import {
  articleWorkerRepository,
  ArticleService,
  browsePublishedPage,
  PublishedArticlePage,
} from "@/infrastructures/article-worker";
import { ReaderFirestoreProvider } from "./firebase-select";
import { PublishStatus } from "@shared/domains/common";

/** Defers Firebase initialization when Article is served by the Worker. */
function firebaseRepository(): ArticleRepository {
  return FirebaseArticleRepository(
    ReaderFirestoreProvider.instance,
    ReaderFirestoreProvider.operations,
  );
}

/** Narrows an optional Cloudflare service binding before issuing requests. */
function isArticleService(value: unknown): value is ArticleService {
  return value !== null && typeof value === "object"
    && "fetch" in value && typeof value.fetch === "function";
}

/** Uses Article Worker when the reader's Cloudflare environment binds it. */
async function activeArticleService(): Promise<ArticleService | null> {
  let env: Record<string, unknown>;
  try {
    env = (await getCloudflareContext({ async: true })).env as Record<string, unknown>;
  } catch (cause) {
    if (process.env.BUILD_TARGET === "cloudflare") throw cause;
    return null;
  }
  const binding: unknown = env.ARTICLE_API;
  if (binding === undefined) return null;
  if (!isArticleService(binding)) throw new Error("ARTICLE_API is not a service binding");
  return binding;
}

/** Selects the current Reader Article repository. */
async function activeReaderRepository(): Promise<Pick<ArticleRepository, "findBySlug" | "search">> {
  const service = await activeArticleService();
  return service === null ? firebaseRepository() : articleWorkerRepository(service);
}

export const ReaderArticleRepositoryProvider = {
  get firebase(): ArticleRepository {
    return firebaseRepository();
  },
  /** Browses one page from Article Worker, preserving Firebase fallback until migration. */
  browse(page: number, size: number) {
    return fromPromise((async (): Promise<PublishedArticlePage> => {
      if (!Number.isSafeInteger(page) || page < 1
        || !Number.isSafeInteger(size) || size < 1 || size > 100) {
        throw new RangeError("Article page and size are out of range");
      }
      const service = await activeArticleService();
      if (service !== null) return browsePublishedPage(service, page, size);
      const criteria = criteriaSchema.parse({ status: PublishStatus.PUBLISHED });
      const articles = await firebaseRepository().search(criteria).unwrap();
      const ordered = [...articles].sort((left, right) =>
        (right.publishedAt?.getTime() ?? 0) - (left.publishedAt?.getTime() ?? 0));
      return {
        articles: ordered.slice((page - 1) * size, page * size),
        total: articles.length,
      };
    })(), (cause) => unexpectedError("Failed to browse Article page", cause));
  },
  current: {
    /** Finds a published article from the configured reader source. */
    findBySlug(slug) {
      return fromPromise(activeReaderRepository(), (cause) =>
        unexpectedError("Failed to select Article repository", cause),
      ).andThen((repository) => repository.findBySlug(slug));
    },
    /** Searches published articles from the configured reader source. */
    search(criteria) {
      return fromPromise(activeReaderRepository(), (cause) =>
        unexpectedError("Failed to select Article repository", cause),
      ).andThen((repository) => repository.search(criteria));
    },
  } satisfies Pick<ArticleRepository, "findBySlug" | "search">,
} as const;
