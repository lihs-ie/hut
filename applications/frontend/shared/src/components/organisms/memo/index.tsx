import { MarkdownRenderer } from "@shared/components/global/mdx";
import { Memo as Aggregate } from "@shared/domains/memo";
import { MemoPresenter } from "./index.presenter";
import { Tag } from "@shared/domains/attributes/tag";
import { ContentType } from "@shared/domains/search-token/reference";
import type { SearchReferenceIdentifier } from "@shared/domains/search-token/reference";
import { ViewTracker } from "@shared/components/molecules/view-tracker";

export type Props = {
  slug: string;
  renderer: MarkdownRenderer;
  findBySlug: (slug: string) => Promise<Aggregate>;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
  incrementViewCount: (identifier: SearchReferenceIdentifier) => Promise<void>;
};

export const Memo = async (props: Props) => {
  const memo = await props.findBySlug(props.slug);

  return (
    <>
      <ViewTracker
        identifier={{ type: ContentType.MEMO, content: memo.identifier }}
        incrementViewCount={props.incrementViewCount}
      />
      <MemoPresenter
        memo={memo}
        renderer={props.renderer}
        findAllTags={props.findAllTags}
      />
    </>
  );
};
