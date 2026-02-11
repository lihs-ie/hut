import { MemoEntry, MemoSlug } from "@shared/domains/memo";
import { MemoEntriesPresenter } from "./list.presenter";
import { MarkdownRenderer } from "@shared/components/global/mdx";

export type Props = {
  getEntries: (slug: MemoSlug) => Promise<MemoEntry[]>;
  slug: MemoSlug;
  renderer: MarkdownRenderer;
};

export const MemoEntries = async (props: Props) => {
  const entries = await props.getEntries(props.slug);

  return <MemoEntriesPresenter entries={entries} renderer={props.renderer} />;
};
