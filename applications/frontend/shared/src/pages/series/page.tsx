import { search } from "@shared/actions/series";
import { findAllTags } from "@shared/actions/tag";
import { SeriesListIndex } from "@shared/components/templates/series/list/index";

export default async function SeriesListPage() {
  const seriesList = await search({ slug: null, tags: null, status: null, freeWord: null });

  return <SeriesListIndex seriesList={seriesList} findAllTags={findAllTags} />;
}
