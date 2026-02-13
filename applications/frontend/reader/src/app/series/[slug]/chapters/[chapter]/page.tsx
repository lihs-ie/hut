import { findBySlug } from "@shared/actions/series";
import { MDXRenderer } from "@shared/components/global/mdx";
import { Chapter } from "@shared/components/organisms/series/chapter";
import { slugSchema } from "@shared/domains/common/slug";

type Props = {
  params: Promise<{ slug: string; chapter: string }>;
};

export default async function SeriesChapterPage(props: Props) {
  const params = await props.params;
  const seriesSlug = slugSchema.parse(params.slug);
  const chapterSlug = slugSchema.parse(params.chapter);

  return (
    <Chapter
      seriesSlug={seriesSlug}
      chapterSlug={chapterSlug}
      findBySlug={findBySlug}
      renderer={MDXRenderer}
    />
  );
}
