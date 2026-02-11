import { Tag, UnvalidatedCriteria } from "@shared/domains/attributes/tag";
import { TagListPresenter } from "./list.presenter";

export type Props = {
  unvalidated: UnvalidatedCriteria;
  search: (unvalidated: UnvalidatedCriteria) => Promise<Tag[]>;
};

export const TagList = async (props: Props) => {
  const tags = await props.search(props.unvalidated);

  return <TagListPresenter tags={tags} />;
};
