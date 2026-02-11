import { Tag, TagIdentifier } from "@shared/domains/attributes/tag";
import { ContentSectionPresenter, LinkMode } from "./search.presenter";
import { Slug } from "@shared/domains/common";
import { ContentType } from "@shared/domains/search-token";
import { Routes } from "@shared/config/presentation/route";

const searchLinks = (type: ContentType) => {
  switch (type) {
    case ContentType.ARTICLE:
      return Routes.page.articles.index;
    case ContentType.MEMO:
      return Routes.page.memos.index;
    // [初期リリース対象外]
    // case ContentType.SERIES:
    //   return Routes.page.series.index;
    default:
      return "#";
  }
};

const buildHref = (
  slug: Slug,
  type: ContentType,
  linkMode: LinkMode,
): string | undefined => {
  if (linkMode === "show") {
    return undefined;
  }
  switch (type) {
    case ContentType.ARTICLE:
      return Routes.page.articles.edit(slug);
    case ContentType.MEMO:
      return Routes.page.memos.edit(slug);
    // [初期リリース対象外]
    // case ContentType.SERIES:
    //   return undefined;
    default:
      return undefined;
  }
};

export type Props<T extends { tags: TagIdentifier[] }> = {
  type: ContentType;
  search: () => Promise<T[]>;
  slugOf: (item: T) => Slug;
  titleOf: (item: T) => string;
  dateOf: (item: T) => Date;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
  hasAllLink?: boolean;
  maxItems?: number;
  linkMode?: LinkMode;
};

export const ContentSection = async <T extends { tags: TagIdentifier[] }>(
  props: Props<T>,
) => {
  const contents = await props.search();
  const contentTags = await Promise.all(
    contents.map((content) => props.findAllTags(content.tags)),
  );
  const linkMode = props.linkMode ?? "show";

  return (
    <ContentSectionPresenter
      type={props.type}
      viewAllLink={props.hasAllLink ? searchLinks(props.type) : undefined}
      title={props.type.toUpperCase()}
      content={contents.map((content, index) => {
        const slug = props.slugOf(content);
        return {
          slug,
          title: props.titleOf(content),
          date: props.dateOf(content),
          type: props.type,
          tagNames: contentTags[index].map((tag) => tag.name),
          href: buildHref(slug, props.type, linkMode),
        };
      })}
      maxItems={props.maxItems}
    />
  );
};
