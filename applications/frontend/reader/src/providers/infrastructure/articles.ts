import { getCloudflareContext } from "@opennextjs/cloudflare";
import { unexpectedError } from "@shared/aspects/error";
import { fromPromise } from "@shared/aspects/result";
import { ArticleRepository } from "@shared/domains/articles";
import { FirebaseArticleRepository } from "@shared/infrastructures/articles";
import { articleWorkerRepository, ArticleService } from "@/infrastructures/article-worker";
import { ReaderFirestoreProvider } from "./firebase-select";

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
async function activeReaderRepository(): Promise<Pick<ArticleRepository, "findBySlug" | "search">> {
  let env: Record<string, unknown>;
  try {
    env = (await getCloudflareContext({ async: true })).env as Record<string, unknown>;
  } catch (cause) {
    if (process.env.BUILD_TARGET === "cloudflare") throw cause;
    return firebaseRepository();
  }
  const binding: unknown = env.ARTICLE_API;
  if (binding === undefined) return firebaseRepository();
  if (!isArticleService(binding)) throw new Error("ARTICLE_API is not a service binding");
  return articleWorkerRepository(binding);
}

export const ReaderArticleRepositoryProvider = {
  get firebase(): ArticleRepository {
    return firebaseRepository();
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
