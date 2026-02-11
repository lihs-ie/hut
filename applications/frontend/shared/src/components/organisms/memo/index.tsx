import { MarkdownRenderer } from "@shared/components/global/mdx";
import { Memo as Aggregate } from "@shared/domains/memo";
import { MemoPresenter } from "./index.presenter";
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

export const Memo = async (props: Props) => {
  const memo = await props.findBySlug(props.slug);

  try {
    await props.incrementViewCount({
      type: ContentType.MEMO,
      content: memo.identifier,
    });
  } catch (_error) {
    void _error;
  }

  return (
    <MemoPresenter
      memo={memo}
      renderer={props.renderer}
      findAllTags={props.findAllTags}
    />
  );
};
