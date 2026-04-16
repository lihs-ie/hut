import { MDXRenderer } from "@shared/components/global/mdx";
import { ChapterIndex } from "@shared/components/templates/series/chapter";
import { findChapterBySlug, findChaptersByIdentifiers } from "@shared/actions/chapter";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug } from "@shared/actions/series";
import type { Metadata } from "next";

type Props = {
  params: Promise<{ slug: string; chapter: string }>;
};

export const generateMetadata = async (props: Props): Promise<Metadata> => {
  const params = await props.params;
  const [series, chapter] = await Promise.all([
    findBySlug(params.slug),
    findChapterBySlug(params.chapter),
  ]);

  const chapterTitle = `${chapter.title} | ${series.title}`;

  return {
    title: chapterTitle,
    description: `${series.title} - ${chapter.title}`,
    authors: [{ name: "lihs" }],
    openGraph: {
      type: "article",
      title: chapterTitle,
      description: `${series.title} - ${chapter.title}`,
      publishedTime: (chapter.publishedAt ?? chapter.timeline.createdAt).toISOString(),
      modifiedTime: chapter.timeline.updatedAt.toISOString(),
    },
    twitter: {
      card: "summary",
      title: chapterTitle,
      description: `${series.title} - ${chapter.title}`,
    },
  };
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
      findChapterBySlug={findChapterBySlug}
      findChaptersByIdentifiers={findChaptersByIdentifiers}
    />
  );
}
