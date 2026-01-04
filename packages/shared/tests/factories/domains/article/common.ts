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
  slugSchema,
  titleSchema,
} from "@/domains/articles";
import { Builder, Characters, Factory, StringFactory } from "../../builder";
import { ulid } from "ulid";
import { DateFactory } from "../common/date";
import {
  ImmutableList,
  ImmutableMap,
  PublishStatus,
  Tag,
} from "@/domains/common";
import { PublishStatusFactory } from "../common/status";
import { fromPromise } from "@/aspects/result";
import {
  AggregateNotFoundError,
  aggregateNotFoundError,
  DuplicationError,
  unexpectedError,
  UnexpectedError,
} from "@/aspects/error";

export type ArticleIdentifierProperties = {
  value: string;
};

export const ArticleIdentifierFactory = Factory<
  ArticleIdentifier,
  ArticleIdentifierProperties
>({
  instantiate: (properties) => articleIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ?? ulid(Builder(DateFactory).buildWith(seed).getTime()),
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type ArticleTitleProperties = {
  value: string;
};

export const TitleFactory = Factory<ArticleTitle, ArticleTitleProperties>({
  instantiate: (properties) => titleSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Builder(StringFactory(1, 100)).buildWith(seed),
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type ArticleContentProperties = {
  value: string;
};

export const ContentFactory = Factory<ArticleContent, ArticleContentProperties>(
  {
    instantiate: (properties) => contentSchema.parse(properties.value),
    prepare: (overrides, seed) => ({
      value: overrides.value ?? Builder(StringFactory(1)).buildWith(seed),
    }),
    retrieve: (instance) => ({
      value: instance,
    }),
  }
);

export type SlugProperties = {
  value: string;
};

export const SlugFactory = Factory<ArticleSlug, SlugProperties>({
  instantiate: (properties) => slugSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ??
      Builder(StringFactory(1, 100, Characters.SLUG)).buildWith(seed),
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type ExcerptProperties = {
  value: string;
};

export const ExcerptFactory = Factory<ArticleExcerpt, ExcerptProperties>({
  instantiate: (properties) => excerptSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Builder(StringFactory(0, 300)).buildWith(seed),
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type ArticleProperties = {
  identifier: ArticleIdentifier;
  title: ArticleTitle;
  content: ArticleContent;
  slug: ArticleSlug;
  status: PublishStatus;
  excerpt: ArticleExcerpt;
  tags: Tag[];
};

export const ArticleFactory = Factory<Article, ArticleProperties>({
  instantiate: (properties) =>
    articleSchema.parse({
      identifier: properties.identifier,
      title: properties.title,
      content: properties.content,
      excerpt: properties.excerpt,
      slug: properties.slug,
      status: properties.status,
      tags: properties.tags,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ?? Builder(ArticleIdentifierFactory).buildWith(seed),
    title: overrides.title ?? Builder(TitleFactory).buildWith(seed),
    content: overrides.content ?? Builder(ContentFactory).buildWith(seed),
    excerpt: overrides.excerpt ?? Builder(ExcerptFactory).buildWith(seed),
    slug: overrides.slug ?? Builder(SlugFactory).buildWith(seed),
    tags: overrides.tags ?? [],
    status:
      overrides.status ??
      (Builder(PublishStatusFactory).buildWith(seed) as PublishStatus),
  }),
  retrieve: (instance) => ({
    identifier: instance.identifier,
    title: instance.title,
    content: instance.content,
    excerpt: instance.excerpt,
    slug: instance.slug,
    status: instance.status,
    tags: instance.tags,
  }),
});

export type ArticleRepositoryFactory = {
  instancies: ImmutableList<Article>;
  onPersist?: (article: Article) => void;
  onTerminate?: (article: Article) => void;
};

export const ArticleRepositoryFactory = Factory<
  ArticleRepository,
  ArticleRepositoryFactory
>({
  instantiate: (properties) => {
    let instancies = ImmutableMap.fromArray(
      properties.instancies
        .map((article): [ArticleIdentifier, Article] => [
          article.identifier,
          article,
        ])
        .toArray()
    );

    const persist: ArticleRepository["persist"] = (article: Article) =>
      fromPromise<void, UnexpectedError | DuplicationError<"Article">>(
        new Promise((resolve) => {
          instancies = instancies.add(article.identifier, article);

          properties.onPersist?.(article);

          resolve();
        }),
        (error) => unexpectedError("Failed to persist article.", error)
      );

    const find: ArticleRepository["find"] = (identifier) =>
      fromPromise<Article, AggregateNotFoundError<"Article">>(
        new Promise((resolve, reject) => {
          instancies.get(identifier).ifPresentOrElse(
            (article) => resolve(article),
            () =>
              reject(
                aggregateNotFoundError(
                  "Article",
                  `Article with identifier ${identifier} not found.`
                )
              )
          );
        }),
        (error) => error as AggregateNotFoundError<"Article">
      );

    const terminate: ArticleRepository["terminate"] = (identifier) =>
      fromPromise<void, AggregateNotFoundError<"Article">>(
        new Promise((resolve, reject) => {
          const articleOption = instancies.get(identifier);

          articleOption.ifPresentOrElse(
            (article) => {
              instancies = instancies.remove(identifier);

              properties.onTerminate?.(article);

              resolve();
            },
            () =>
              reject(
                aggregateNotFoundError(
                  "Article",
                  `Article with identifier ${identifier} not found.`
                )
              )
          );
        }),
        (error) => error as AggregateNotFoundError<"Article">
      );

    const search: ArticleRepository["search"] = (criteria) =>
      fromPromise<Article[], UnexpectedError>(
        new Promise((resolve) => {
          const results = instancies
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
                  article.tags.includes(tag)
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
        (error) => unexpectedError("Failed to search articles.", error)
      );

    return {
      find,
      persist,
      terminate,
      search,
    };
  },
  prepare: (overrides, seed) => ({
    instancies:
      overrides.instancies ?? Builder(ArticleFactory).buildListWith(10, seed),
    onPersist: overrides.onPersist,
    onTerminate: overrides.onTerminate,
  }),
  retrieve: () => {
    throw new Error("Repository cannot be retrieved.");
  },
});
