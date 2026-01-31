import { PublishStatus } from "@shared/domains/common";
import { MemoEditSidebarPresenter } from "./sidebar.presenter.";
import {
  Memo,
  MemoSnapshot,
  toSnapshot,
  UnvalidatedMemo,
} from "@shared/domains/memo";

export type Props = {
  slug: string;
  findBySlug: (slug: string) => Promise<Memo>;
  edit: (unvalidaed: UnvalidatedMemo, before: MemoSnapshot) => Promise<void>;
};

const changeStatus = (snapshot: MemoSnapshot, edit: Props["edit"]) => {
  return async (next: PublishStatus) => {
    "use server";

    const unvalidated: UnvalidatedMemo = {
      identifier: snapshot.identifier,
      title: snapshot.title,
      slug: snapshot.slug,
      entries: snapshot.entries,
      tags: snapshot.tags,
      status: next,
      timeline: {
        createdAt: snapshot.timeline.createdAt,
        updatedAt: new Date(),
      },
    };

    await edit(unvalidated, snapshot);
  };
};

export const MemoEditSidebar = async (props: Props) => {
  const memo = await props.findBySlug(props.slug);

  return (
    <MemoEditSidebarPresenter
      initialStatus={memo.status}
      changeStatus={changeStatus(toSnapshot(memo), props.edit)}
    />
  );
};
