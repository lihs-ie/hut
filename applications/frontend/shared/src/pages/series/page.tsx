import { search } from "@shared/actions/series";
import { findAllTags } from "@shared/actions/tag";
import { SeriesListIndex } from "@shared/components/templates/series/list/index";

export default async function SeriesListPage() {
  return (
    <SeriesListIndex
      search={async () =>
        await search({ slug: null, tags: null, status: null, freeWord: null })
      }
      findAllTags={findAllTags}
    />
  );
}
