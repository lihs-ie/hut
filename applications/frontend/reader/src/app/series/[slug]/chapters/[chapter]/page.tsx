import { MDXRenderer } from "@shared/components/global/mdx";
import { ChapterIndex } from "@shared/components/templates/series/chapter";
import { findChapterBySlug, findChaptersByIdentifiers } from "@shared/actions/chapter";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug, searchAllSlugs } from "@/actions/series";

export const revalidate = 3600;

type Props = {
  params: Promise<{ slug: string; chapter: string }>;
};

export async function generateStaticParams() {
  const slugs = await searchAllSlugs();

  const paramsList = await Promise.all(
    slugs.map(async (slug) => {
      const series = await findBySlug(slug);
      const chapters = await findChaptersByIdentifiers(series.chapters);
      return chapters.map((chapter) => ({ slug, chapter: chapter.slug }));
    })
  );

  return paramsList.flat();
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
