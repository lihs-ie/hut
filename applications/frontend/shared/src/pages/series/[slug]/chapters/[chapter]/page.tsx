import { MDXRenderer } from "@shared/components/global/mdx";
import { ChapterIndex } from "@shared/components/templates/series/chapter";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug } from "@shared/actions/series";

type Props = {
  params: Promise<{ slug: string; chapter: string }>;
};

export default async function ChapterPage(props: Props) {
  const params = await props.params;
  const series = await findBySlug(params.slug);

  return (
    <ChapterIndex
      slug={slugSchema.parse(params.slug)}
      chapterSlug={slugSchema.parse(params.chapter)}
      series={series}
      renderer={MDXRenderer}
    />
  );
}
