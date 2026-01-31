import { ContentSection } from "@shared/components/organisms/common/top/search";
import { Tag } from "@shared/domains/attributes/tag";
import { ContentType } from "@shared/domains/search-token";
import { Series } from "@shared/domains/series";

export type Props = {
  search: () => Promise<Series[]>;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
};

export const SeriesListIndex = (props: Props) => (
  <ContentSection
    search={props.search}
    findAllTags={props.findAllTags}
    slugOf={(series) => series.slug}
    titleOf={(series) => series.title}
    dateOf={(series) => series.timeline.createdAt}
    type={ContentType.SERIES}
  />
);
