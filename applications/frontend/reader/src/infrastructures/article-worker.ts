import { aggregateNotFoundError, isAggregateNotFoundError, unexpectedError } from "@shared/aspects/error";
import { fromPromise } from "@shared/aspects/result";
import { Article, ArticleRepository, validateArticle } from "@shared/domains/articles";
import { PublishStatus } from "@shared/domains/common";
import { z } from "zod";

export type ArticleService = {
  fetch(request: Request): Promise<Response>;
};

const publishedViewSchema = z.object({
  identifier: z.string(),
  phase: z.literal("published"),
  title: z.string(),
  body: z.string(),
  slug: z.string(),
  excerpt: z.string(),
  tags: z.array(z.string()),
  images: z.array(z.string()),
  createdAt: z.iso.datetime(),
  updatedAt: z.iso.datetime(),
  publishedAt: z.iso.datetime(),
});

const articlePageSchema = z.object({
  articles: z.array(publishedViewSchema),
  pagination: z.object({ total: z.number().int().nonnegative() }),
});

type PublishedView = z.infer<typeof publishedViewSchema>;

/** Converts a published Article API view into the current reader display model. */
function toReaderArticle(view: PublishedView): Article {
  const result = validateArticle({
    identifier: view.identifier,
    title: view.title,
    content: view.body,
    excerpt: view.excerpt,
    slug: view.slug,
    status: PublishStatus.PUBLISHED,
    tags: view.tags,
    images: view.images,
    publishedAt: new Date(view.publishedAt),
    timeline: {
      createdAt: new Date(view.createdAt),
      updatedAt: new Date(view.updatedAt),
    },
  });
  if (result.isErr) {
    throw new Error("Article API returned an invalid published article");
  }
  return result.unwrap();
}

/** Fetches an Article API response without forwarding browser credentials. */
async function requestArticle(service: ArticleService, path: string): Promise<Response> {
  return service.fetch(new Request(new URL(path, "https://article.internal")));
}

/** Retries a changing offset-based listing instead of returning duplicate or incomplete pages. */
async function loadPublishedArticles(service: ArticleService): Promise<Article[]> {
  for (let attempt = 0; attempt < 3; attempt += 1) {
    const articles: Article[] = [];
    const seen = new Set<string>();
    let expectedTotal: number | undefined;
    let changed = false;
    for (let page = 1; ; page += 1) {
      const response = await requestArticle(service, `/articles?page=${page}&size=100`);
      if (!response.ok) {
        throw new Error(`Article API returned HTTP ${response.status}`);
      }
      const result = articlePageSchema.parse(await response.json());
      expectedTotal ??= result.pagination.total;
      if (result.pagination.total !== expectedTotal) {
        changed = true;
        break;
      }
      for (const view of result.articles) {
        if (seen.has(view.identifier)) {
          changed = true;
          break;
        }
        seen.add(view.identifier);
        articles.push(toReaderArticle(view));
      }
      if (changed) break;
      if (articles.length === expectedTotal) return articles;
      if (articles.length > expectedTotal || result.articles.length === 0) {
        changed = true;
        break;
      }
    }
  }
  throw new Error("Article API pages changed during the read");
}

/** Applies the reader's existing search criteria to a published-only page. */
function selectArticles(articles: Article[], criteria: Parameters<ArticleRepository["search"]>[0]): Article[] {
  if (criteria.status && criteria.status !== PublishStatus.PUBLISHED) {
    return [];
  }
  const keyword = criteria.freeWord?.toLowerCase();
  const selected = articles.filter((article) => {
    if (criteria.slug && article.slug !== criteria.slug) return false;
    if (criteria.tags?.length && !criteria.tags.some((tag) => article.tags.includes(tag))) return false;
    if (!keyword) return true;
    return [article.title, article.content, article.excerpt]
      .some((value) => value.toLowerCase().includes(keyword));
  });
  const field = criteria.sortBy ?? "createdAt";
  const direction = criteria.order === "asc" ? 1 : -1;
  return selected.sort((left, right) => {
    const leftDate = field === "updatedAt" ? left.timeline.updatedAt : left.timeline.createdAt;
    const rightDate = field === "updatedAt" ? right.timeline.updatedAt : right.timeline.createdAt;
    return direction * (leftDate.getTime() - rightDate.getTime());
  });
}

/** Reader-only repository backed by the private Article Worker service binding. */
export function articleWorkerRepository(
  service: ArticleService,
): Pick<ArticleRepository, "findBySlug" | "search"> {
  return {
    /** Reads a published article by its public slug. */
    findBySlug(slug) {
      return fromPromise(
        (async () => {
          const response = await requestArticle(service, `/articles/${encodeURIComponent(slug)}`);
          if (response.status === 404) {
            throw aggregateNotFoundError("Article", `Article ${slug} not found.`);
          }
          if (!response.ok) {
            throw new Error(`Article API returned HTTP ${response.status}`);
          }
          return toReaderArticle(publishedViewSchema.parse(await response.json()));
        })(),
        (cause) => isAggregateNotFoundError(cause)
          ? cause
          : unexpectedError("Failed to read Article API", cause),
      );
    },
    /** Reads all published pages before applying the reader's local filters. */
    search(criteria) {
      return fromPromise(
        (async () => {
          if (criteria.status && criteria.status !== PublishStatus.PUBLISHED) return [];
          if (criteria.slug) {
            const response = await requestArticle(
              service,
              `/articles/${encodeURIComponent(criteria.slug)}`,
            );
            if (response.status === 404) return [];
            if (!response.ok) {
              throw new Error(`Article API returned HTTP ${response.status}`);
            }
            const article = toReaderArticle(
              publishedViewSchema.parse(await response.json()),
            );
            return selectArticles([article], criteria);
          }
          return selectArticles(await loadPublishedArticles(service), criteria);
        })(),
        (cause) => unexpectedError("Failed to search Article API", cause),
      );
    },
  };
}
