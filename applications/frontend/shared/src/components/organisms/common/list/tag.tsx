import { Tag, TagIdentifier } from "@shared/domains/attributes/tag";
import { TagBadgeListPresenter } from "./tag.presenter";

export type Props = {
  identifiers: TagIdentifier[];
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
};

export const TagBadgeList = async (props: Props) => {
  const tags = await props.findAllTags(props.identifiers);

  return (
    <TagBadgeListPresenter
      tags={tags.map((tag) => ({ name: tag.name, logo: tag.logo }))}
    />
  );
};
