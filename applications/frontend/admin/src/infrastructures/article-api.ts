import { z } from "zod";

const articlePhaseSchema = z.enum([
  "unvalidated",
  "proofreaded",
  "ready",
  "published",
  "private",
]);

export const articleViewSchema = z.object({
  identifier: z.string(),
  phase: articlePhaseSchema,
  title: z.string(),
  body: z.string(),
  slug: z.string().nullable(),
  excerpt: z.string().nullable(),
  tags: z.array(z.string()),
  images: z.array(z.string()),
  createdAt: z.iso.datetime(),
  updatedAt: z.iso.datetime(),
  publishedAt: z.iso.datetime().nullable(),
});

export type ArticleView = z.infer<typeof articleViewSchema>;
export type ArticlePhase = z.infer<typeof articlePhaseSchema>;

const articlePageSchema = z.object({
  articles: z.array(articleViewSchema),
  pagination: z.object({
    total: z.number().int().nonnegative(),
    items: z.number().int().nonnegative(),
    current: z.number().int().positive(),
    firstPage: z.number().int().nonnegative(),
    lastPage: z.number().int().nonnegative(),
  }),
});

const proofreadResponseSchema = z.object({
  article: z.string(),
  phase: z.literal("proofreaded"),
});

const regenerationResponseSchema = z.object({
  article: z.string(),
  requestIdentifier: z.string(),
});

export type DraftInput = {
  title: string;
  body: string;
  slug: string | null;
  tags: string[];
};

export type ArticleTransport = {
  fetch(request: Request): Promise<Response>;
};

/** Preserves the API's public error code for presentation-layer handling. */
export class ArticleApiError extends Error {
  constructor(
    readonly status: number,
    readonly code: string | null,
    readonly correlation: string | null,
  ) {
    super(`Article API returned HTTP ${status}${code ? ` (${code})` : ""}`);
    this.name = "ArticleApiError";
  }
}

/** Creates a typed Article admin client over a caller-provided transport. */
export function articleAdminApi(
  transport: ArticleTransport,
  baseUrl: string,
  actor: string,
) {
  /** Sends one admin request and validates its JSON response. */
  async function request<T>(
    method: string,
    path: string,
    schema: z.ZodType<T>,
    body?: object,
  ): Promise<T> {
    const headers = new Headers({ "X-Hut-Actor": actor });
    if (body !== undefined) headers.set("Content-Type", "application/json");
    const response = await transport.fetch(new Request(new URL(path, baseUrl), {
      method,
      headers,
      body: body === undefined ? undefined : JSON.stringify(body),
    }));
    if (!response.ok) {
      throw new ArticleApiError(
        response.status,
        response.headers.get("X-Article-Error-Code"),
        response.headers.get("X-Correlation-Identifier"),
      );
    }
    return schema.parse(await response.json());
  }

  /** Encodes an aggregate route without allowing an identifier to change the path. */
  function articlePath(articleIdentifier: string): string {
    return `/admin/articles/${encodeURIComponent(articleIdentifier)}`;
  }

  return {
    /** Starts an unvalidated draft. */
    jotDown: (draft: DraftInput) =>
      request("POST", "/admin/articles", articleViewSchema, draft),
    /** Lists articles in the requested lifecycle phase. */
    browse: (page: number, size: number, status?: ArticlePhase) => {
      const query = new URLSearchParams({ page: String(page), size: String(size) });
      if (status !== undefined) query.set("status", status);
      return request("GET", `/admin/articles?${query}`, articlePageSchema);
    },
    /** Reads an aggregate by its identifier. */
    view: (articleIdentifier: string) =>
      request("GET", articlePath(articleIdentifier), articleViewSchema),
    /** Replaces draft input and returns it to the unvalidated phase. */
    amendDraft: (articleIdentifier: string, draft: DraftInput) =>
      request("PUT", `${articlePath(articleIdentifier)}/draft`, articleViewSchema, draft),
    /** Marks a draft as proofread. */
    proofread: (articleIdentifier: string) =>
      request("POST", `${articlePath(articleIdentifier)}/proofreading`, proofreadResponseSchema),
    /** Revises the generated excerpt. */
    reviseExcerpt: (articleIdentifier: string, excerpt: string) =>
      request("PATCH", `${articlePath(articleIdentifier)}/excerpt`, articleViewSchema, { excerpt }),
    /** Publishes a ready article. */
    publish: (articleIdentifier: string) =>
      request("POST", `${articlePath(articleIdentifier)}/publication`, articleViewSchema),
    /** Takes a published article offline. */
    takeDown: (articleIdentifier: string) =>
      request("DELETE", `${articlePath(articleIdentifier)}/publication`, articleViewSchema),
    /** Restores a private article to the publication-ready draft phase. */
    resumePublication: (articleIdentifier: string) =>
      request("POST", `${articlePath(articleIdentifier)}/publication-resumptions`, articleViewSchema),
    /** Physically discards an article. */
    discard: (articleIdentifier: string) =>
      request("DELETE", articlePath(articleIdentifier), z.object({ article: z.string() })),
    /** Checks whether a slug is free for the given article. */
    checkSlug: (articleIdentifier: string, slug: string) =>
      request(
        "GET",
        `${articlePath(articleIdentifier)}/slug-availability?${new URLSearchParams({ slug })}`,
        z.object({ available: z.boolean() }),
      ),
    /** Requests a new asynchronous excerpt generation. */
    regenerateExcerpt: (articleIdentifier: string) =>
      request(
        "POST",
        `${articlePath(articleIdentifier)}/excerpt-generation-requests`,
        regenerationResponseSchema,
      ),
  };
}
