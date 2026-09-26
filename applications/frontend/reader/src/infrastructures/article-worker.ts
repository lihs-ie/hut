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
          const articles: Article[] = [];
          for (let page = 1; ; page += 1) {
            const response = await requestArticle(service, `/articles?page=${page}&size=100`);
            if (!response.ok) {
              throw new Error(`Article API returned HTTP ${response.status}`);
            }
            const result = articlePageSchema.parse(await response.json());
            articles.push(...result.articles.map(toReaderArticle));
            if (articles.length >= result.pagination.total) break;
            if (result.articles.length === 0) {
              throw new Error("Article API returned an incomplete page");
            }
          }
          return selectArticles(articles, criteria);
        })(),
        (cause) => unexpectedError("Failed to search Article API", cause),
      );
    },
  };
}
