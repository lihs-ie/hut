import { MDXRenderer } from "@shared/components/global/mdx";
import { ChapterIndex } from "@shared/components/templates/series/chapter";
import { findChapterBySlug, findChaptersByIdentifiers } from "@shared/actions/chapter";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug, searchAllChapterParams } from "@/actions/series";

export const revalidate = 3600;

type Props = {
  params: Promise<{ slug: string; chapter: string }>;
};

export async function generateStaticParams() {
  return await searchAllChapterParams();
}

export default async function ChapterPage(props: Props) {
  const params = await props.params;
  const series = await findBySlug(params.slug);

  return (
    <ChapterIndex
      slug={slugSchema.parse(params.slug)}
      chapterSlug={slugSchema.parse(params.chapter)}
      series={series}
      renderer={MDXRenderer}
      findChapterBySlug={findChapterBySlug}
      findChaptersByIdentifiers={findChaptersByIdentifiers}
    />
  );
}
