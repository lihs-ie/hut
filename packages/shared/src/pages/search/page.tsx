import { SearchIndex } from "@shared/components/templates/search";
import { getAllTags, findAllTags, ofNames } from "@shared/actions/tag";
import { searchByToken } from "@shared/actions/search-token";

type SearchParams = {
  freeWord?: string;
  tags?: string;
  type?: string;
  sortBy?: string;
  order?: string;
};

type Props = {
  searchParams: Promise<SearchParams>;
};

const parseAsArray = (values: string): string[] => {
  return values.split(",").map((value) => value.trim());
};

export default async function SearchPage(props: Props) {
  const parameters = await props.searchParams;

  return (
    <SearchIndex
      search={searchByToken}
      getAllTags={getAllTags}
      findAllTags={findAllTags}
      ofNamesTags={ofNames}
      unvalidatedCriteria={{
        freeWord: parameters.freeWord ?? null,
        tags: parameters.tags ? parseAsArray(parameters.tags) : null,
        type: parameters.type ?? null,
        sortBy: parameters.sortBy ?? null,
        order: parameters.order ?? null,
        limit: 10,
      }}
    />
  );
}
