import { Article as Aggregate } from "@shared/domains/articles";
import { MarkdownRenderer } from "@shared/components/global/mdx";
import { ContentPresenter } from "../common/content.presenter";
import { Tag } from "@shared/domains/attributes/tag";

export type Props = {
  slug: string;
  renderer: MarkdownRenderer;
  findBySlug: (slug: string) => Promise<Aggregate>;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
};

export const revalidate = 3600;

export const Article = async (props: Props) => {
  const article = await props.findBySlug(props.slug);

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
