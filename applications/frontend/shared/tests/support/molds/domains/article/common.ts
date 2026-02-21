import {
  Article,
  ArticleContent,
  ArticleExcerpt,
  ArticleIdentifier,
  articleIdentifierSchema,
  ArticleRepository,
  articleSchema,
  ArticleSlug,
  ArticleTitle,
  contentSchema,
  excerptSchema,
  titleSchema,
} from "@shared/domains/articles";
import { ulid } from "ulid";
import { DateMold, TimelineMold } from "../common/date";
import { ImmutableMap, PublishStatus, Timeline } from "@shared/domains/common";
import { PublishStatusMold } from "../common/status";
import { fromPromise } from "@shared/aspects/result";
import {
  AggregateNotFoundError,
  aggregateNotFoundError,
  DuplicationError,
  unexpectedError,
  UnexpectedError,
} from "@shared/aspects/error";
import { Forger, Mold, StringMold } from "@lihs-ie/forger-ts";
import { TagIdentifier } from "@shared/domains/attributes/tag";
import { TagIdentifierMold } from "../attributes/tag";
import { SlugMold, SlugProperties } from "../common/slug";
import { ImageIdentifier } from "@shared/domains/image";
import { ImageIdentifierMold } from "../image";

export type ArticleIdentifierProperties = {
  value: string;
};

export const ArticleIdentifierMold = Mold<
  ArticleIdentifier,
  ArticleIdentifierProperties
>({
  pour: (properties) => articleIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ?? ulid(Forger(DateMold).forgeWithSeed(seed).getTime()),
  }),
});

export type ArticleTitleProperties = {
  value: string;
};

export const TitleMold = Mold<ArticleTitle, ArticleTitleProperties>({
  pour: (properties) => titleSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Forger(StringMold(1, 100)).forgeWithSeed(seed),
  }),
});

export type ArticleContentProperties = {
  value: string;
};

export const ContentMold = Mold<ArticleContent, ArticleContentProperties>({
  pour: (properties) => contentSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Forger(StringMold(1)).forgeWithSeed(seed),
  }),
});

export { SlugMold };
export type { SlugProperties };

export type ExcerptProperties = {
  value: string;
};

export const ExcerptMold = Mold<ArticleExcerpt, ExcerptProperties>({
  pour: (properties) => excerptSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Forger(StringMold(0, 300)).forgeWithSeed(seed),
  }),
});

export type ArticleProperties = {
  identifier: ArticleIdentifier;
  title: ArticleTitle;
  content: ArticleContent;
  slug: ArticleSlug;
  status: PublishStatus;
  excerpt: ArticleExcerpt;
  tags: TagIdentifier[];
  images: ImageIdentifier[];
  timeline: Timeline;
};

export const ArticleMold = Mold<Article, ArticleProperties>({
  pour: (properties) =>
    articleSchema.parse({
      identifier: properties.identifier,
      title: properties.title,
      content: properties.content,
      excerpt: properties.excerpt,
      slug: properties.slug,
      status: properties.status,
      tags: properties.tags,
      images: properties.images,
      timeline: properties.timeline,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ?? Forger(ArticleIdentifierMold).forgeWithSeed(seed),
    title: overrides.title ?? Forger(TitleMold).forgeWithSeed(seed),
    content: overrides.content ?? Forger(ContentMold).forgeWithSeed(seed),
    excerpt: overrides.excerpt ?? Forger(ExcerptMold).forgeWithSeed(seed),
    slug: overrides.slug ?? Forger(SlugMold).forgeWithSeed(seed),
    tags:
      overrides.tags ?? Forger(TagIdentifierMold).forgeMultiWithSeed(3, seed),
    status: overrides.status ?? Forger(PublishStatusMold).forgeWithSeed(seed),
    images:
      overrides.images ??
      Forger(ImageIdentifierMold).forgeMultiWithSeed(5, seed),
    timeline: overrides.timeline ?? Forger(TimelineMold).forgeWithSeed(seed),
  }),
});


export type ArticleRepositoryMold = {
  instances: Article[];
  onPersist?: (article: Article) => void;
  onTerminate?: (article: Article) => void;
};

export const ArticleRepositoryMold = Mold<
  ArticleRepository,
  ArticleRepositoryMold
>({
  pour: (properties) => {
    let instances = ImmutableMap.fromArray(
      properties.instances.map((article): [ArticleIdentifier, Article] => [
        article.identifier,
        article,
      ]),
    );

    const persist: ArticleRepository["persist"] = (article: Article) =>
      fromPromise<void, UnexpectedError | DuplicationError<"Article">>(
        new Promise((resolve) => {
          instances = instances.add(article.identifier, article);

          properties.onPersist?.(article);

          resolve();
        }),
        (error) => unexpectedError("Failed to persist article.", error),
      );

    const find: ArticleRepository["find"] = (identifier) =>
      fromPromise<Article, AggregateNotFoundError<"Article">>(
        new Promise((resolve, reject) => {
          instances.get(identifier).ifPresentOrElse(
            (article) => resolve(article),
            () =>
              reject(
                aggregateNotFoundError(
                  "Article",
                  `Article with identifier ${identifier} not found.`,
                ),
              ),
          );
        }),
        (error) => error as AggregateNotFoundError<"Article">,
      );

    const terminate: ArticleRepository["terminate"] = (identifier) =>
      fromPromise<void, AggregateNotFoundError<"Article">>(
        new Promise((resolve, reject) => {
          const articleOption = instances.get(identifier);

          articleOption.ifPresentOrElse(
            (article) => {
              instances = instances.remove(identifier);
              properties.onTerminate?.(article);

              resolve();
            },
            () =>
              reject(
                aggregateNotFoundError(
                  "Article",
                  `Article with identifier ${identifier} not found.`,
                ),
              ),
          );
        }),
        (error) => error as AggregateNotFoundError<"Article">,
      );

    const findBySlug: ArticleRepository["findBySlug"] = (slug) =>
      fromPromise<Article, AggregateNotFoundError<"Article"> | UnexpectedError>(
        new Promise((resolve, reject) => {
          const found = instances
            .values()
            .find((article) => article.slug === slug);

          if (found) {
            resolve(found);
          } else {
            reject(
              aggregateNotFoundError(
                "Article",
                `Article with slug ${slug} not found.`,
              ),
            );
          }
        }),
        (error) => error as AggregateNotFoundError<"Article"> | UnexpectedError,
      );

    const ofIdentifiers: ArticleRepository["ofIdentifiers"] = (
      identifiers,
      throwOnMissing = false,
    ) =>
      fromPromise<
        Article[],
        UnexpectedError | AggregateNotFoundError<"Article">
      >(
        new Promise((resolve, reject) => {
          const articles: Article[] = [];

          for (const identifier of identifiers) {
            const target = instances.get(identifier);

            if (!target.isPresent()) {
              if (throwOnMissing) {
                reject(
                  aggregateNotFoundError(
                    "Article",
                    `Article with identifier ${identifier} not found.`,
                  ),
                );
                return;
              }
              continue;
            }

            target.ifPresent((article) => {
              articles.push(article);
            });
          }

          resolve(articles);
        }),
        (error) => error as UnexpectedError | AggregateNotFoundError<"Article">,
      );

    const search: ArticleRepository["search"] = (criteria) =>
      fromPromise<Article[], UnexpectedError>(
        new Promise((resolve) => {
          const results = instances
            .filter((_, article) => {
              if (criteria.slug && article.slug !== criteria.slug) {
                return false;
              }

              if (criteria.status && article.status !== criteria.status) {
                return false;
              }

              if (criteria.freeWord) {
                const keyword = criteria.freeWord.toLowerCase();
                if (
                  !(
                    article.title.toLowerCase().includes(keyword) ||
                    article.content.toLowerCase().includes(keyword) ||
                    article.excerpt.toLowerCase().includes(keyword)
                  )
                ) {
                  return false;
                }
              }

              if (criteria.tags && criteria.tags.length > 0) {
                const hasTag = criteria.tags.some((tag) =>
                  article.tags.includes(tag),
                );

                if (!hasTag) {
                  return false;
                }
              }

              return true;
            })
            .values();

          resolve(results);
        }),
        (error) => unexpectedError("Failed to search articles.", error),
      );

    return {
      find,
      findBySlug,
      ofIdentifiers,
      persist,
      terminate,
      search,
    };
  },
  prepare: (overrides, seed) => ({
    instances:
      overrides.instances ?? Forger(ArticleMold).forgeMultiWithSeed(10, seed),
    onPersist: overrides.onPersist,
    onTerminate: overrides.onTerminate,
  }),
});
