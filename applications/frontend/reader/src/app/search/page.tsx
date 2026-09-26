import { SearchIndex } from "@shared/components/templates/search";
import { getAllTags, findAllTags, ofNames } from "@/actions/tag";
import { searchByToken } from "@/actions/search-token";

type Props = {
  searchParams: Promise<{
    freeWord?: string;
    tags?: string;
    type?: string;
    sortBy?: string;
    order?: string;
  }>;
};

/** Searches published articles. */
export default async function Page({ searchParams }: Props) {
  const parameters = await searchParams;
  return (
    <SearchIndex
      search={searchByToken}
      getAllTags={getAllTags}
      findAllTags={findAllTags}
      ofNamesTags={ofNames}
      unvalidatedCriteria={{
        freeWord: parameters.freeWord?.slice(0, 100) ?? null,
        tags: parameters.tags
          ? parameters.tags.split(",").map((value) => value.trim())
          : null,
        type: parameters.type ?? null,
        sortBy: parameters.sortBy ?? null,
        order: parameters.order ?? null,
        limit: 10,
      }}
    />
  );
}
