import { Tag } from "@shared/domains/attributes/tag";
import { SearchFilterPresenter } from "./filters.presenter";

export type Props = {
  getAllTags: () => Promise<Tag[]>;
};

export const SearchFilter = async (props: Props) => {
  const tags = await props.getAllTags();

  return <SearchFilterPresenter tags={tags} />;
};
