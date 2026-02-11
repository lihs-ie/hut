import { UnvalidatedCriteria } from "@shared/domains/search-token";
import { SearchResultPresenter } from "./result.presenter";
import { Memo } from "@shared/domains/memo";
import { Series } from "@shared/domains/series";
import { Article } from "@shared/domains/articles";
import { Tag } from "@shared/domains/attributes/tag";

export type Props = {
  search: (
    unvalidated: UnvalidatedCriteria,
  ) => Promise<(Article | Series | Memo)[]>;
  unvalidatedCriteria: UnvalidatedCriteria;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
  ofNamesTags: (names: string[]) => Promise<Tag[]>;
};

export const SearchResult = async (props: Props) => {
  const selectedTags = props.unvalidatedCriteria.tags
    ? await props.ofNamesTags(props.unvalidatedCriteria.tags)
    : [];

  const contents = await props.search({
    ...props.unvalidatedCriteria,
    tags: selectedTags.map((tag) => tag.identifier),
  });
  const contentTags = await Promise.all(
    contents.map((content) => props.findAllTags(content.tags)),
  );

  return (
    <SearchResultPresenter
      contents={contents.map((content, index) => ({
        ...content,
        tagNames: contentTags[index].map((tag) => tag.name),
      }))}
      criteria={props.unvalidatedCriteria}
    />
  );
};
