import { MarkdownRenderer } from "@shared/components/global/mdx";
import { Memo as Aggregate } from "@shared/domains/memo";
import { MemoPresenter } from "./index.presenter";
import { Tag } from "@shared/domains/attributes/tag";

export type Props = {
  slug: string;
  renderer: MarkdownRenderer;
  findBySlug: (slug: string) => Promise<Aggregate>;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
};

export const revalidate = 3600;

export const Memo = async (props: Props) => {
  const memo = await props.findBySlug(props.slug);

  return (
    <MemoPresenter
      memo={memo}
      renderer={props.renderer}
      findAllTags={props.findAllTags}
    />
  );
};
