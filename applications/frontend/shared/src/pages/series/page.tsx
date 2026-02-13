import { search } from "@shared/actions/series";
import { SeriesListIndex } from "@shared/components/templates/series/list";

export default async function SeriesListPage() {
  const seriesList = await search({ slug: null, tags: null });

  return <SeriesListIndex seriesList={seriesList} />;
}
