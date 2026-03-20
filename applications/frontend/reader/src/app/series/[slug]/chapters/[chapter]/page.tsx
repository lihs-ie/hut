import { MDXRenderer } from "@shared/components/global/mdx";
import { ChapterIndex } from "@shared/components/templates/series/chapter";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug, searchAllSlugs } from "@/actions/series";

export const revalidate = 3600;

type Props = {
  params: Promise<{ slug: string; chapter: string }>;
};

export async function generateStaticParams() {
  const slugs = await searchAllSlugs();
  const params: { slug: string; chapter: string }[] = [];

  for (const slug of slugs) {
    const series = await findBySlug(slug);
    for (const chapter of series.chapters) {
      params.push({ slug, chapter: chapter.slug });
    }
  }

  return params;
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
    />
  );
}
