import { SearchIndex } from "@shared/components/templates/search";
import { getAllTags, findAllTags, ofNames } from "@shared/actions/tag";
import { search as searchArticles } from "@shared/actions/article";
import { UnvalidatedCriteria } from "@shared/domains/search-token";

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

const search = (criteria: UnvalidatedCriteria) =>
  criteria.type && criteria.type !== "article"
    ? Promise.resolve([])
    : searchArticles({
        freeWord: criteria.freeWord,
        tags: criteria.tags,
        sortBy: criteria.sortBy === "latest" ? "updatedAt" : "createdAt",
        order: criteria.sortBy === "oldest" ? "asc" : "desc",
      });

export default async function SearchPage(props: Props) {
  const parameters = await props.searchParams;

  return (
    <SearchIndex
      search={search}
      getAllTags={getAllTags}
      findAllTags={findAllTags}
      ofNamesTags={ofNames}
      unvalidatedCriteria={{
        freeWord: parameters.freeWord?.slice(0, 100) ?? null,
        tags: parameters.tags ? parseAsArray(parameters.tags) : null,
        type: parameters.type ?? null,
        sortBy: parameters.sortBy ?? null,
        order: parameters.order ?? null,
        limit: 10,
      }}
    />
  );
}
