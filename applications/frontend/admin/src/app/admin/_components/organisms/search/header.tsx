import { ContentType } from "@shared/domains/search-token";
import { Routes } from "@/config/routes";

import { Tag } from "@shared/domains/attributes/tag";
import { AdminSearchHeaderPresenter } from "./header.presenter";

export type Props<Criteria> = {
  title: string;
  contentType: ContentType;
  unvalidated: Criteria;
  getAllTags: () => Promise<Tag[]>;
};

const newContentPath = (contentType: ContentType) => {
  switch (contentType) {
    case ContentType.ARTICLE:
      return Routes.page.articles.new;
    case ContentType.MEMO:
      return Routes.page.memos.new;
    case ContentType.SERIES:
      return Routes.page.series.new;
    default:
      return Routes.admin.dashboard;
  }
};

export const AdminSearchHeader = async <Criteria,>(props: Props<Criteria>) => {
  const tags = await props.getAllTags();

  return (
    <AdminSearchHeaderPresenter
      {...props}
      tagChoices={tags}
      newContentPath={newContentPath(props.contentType)}
    />
  );
};
