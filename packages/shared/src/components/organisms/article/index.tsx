import { Article as Aggregate } from "@shared/domains/articles";
import { MarkdownRenderer } from "@shared/components/global/mdx";
import { ContentPresenter } from "../common/content.presenter";
import { Tag } from "@shared/domains/attributes/tag";
import { ContentType } from "@shared/domains/search-token/reference";
import type { SearchReferenceIdentifier } from "@shared/domains/search-token/reference";

export type Props = {
  slug: string;
  renderer: MarkdownRenderer;
  findBySlug: (slug: string) => Promise<Aggregate>;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
  incrementViewCount: (identifier: SearchReferenceIdentifier) => Promise<void>;
};

export const revalidate = 3600;

export const Article = async (props: Props) => {
  const article = await props.findBySlug(props.slug);

  try {
    await props.incrementViewCount({
      type: ContentType.ARTICLE,
      content: article.identifier,
    });
  } catch (_error) {
    void _error;
  }

  return (
    <ContentPresenter
      target={article}
      tagOf={(article) => article.tags}
      contentOf={(article) => article.content}
      renderer={props.renderer}
      findAllTags={props.findAllTags}
    />
  );
};
