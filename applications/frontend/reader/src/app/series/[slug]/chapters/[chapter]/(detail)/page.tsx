import { Suspense } from "react";
import { MDXRenderer } from "@shared/components/global/mdx";
import { ArticleContentSkeleton } from "@shared/components/molecules/skeleton";
import { ChapterContent } from "@shared/components/organisms/series/chapter/content";
import { findChapterBySlug, findPublishedChaptersByIdentifiers } from "@/actions/chapter";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug, searchAllChapterParams } from "@/actions/series";
import { isPublished } from "@shared/domains/common";
import type { Metadata } from "next";

export const revalidate = 3600;

type Props = {
  params: Promise<{ slug: string; chapter: string }>;
};

type ContentProps = {
  params: Promise<{ slug: string; chapter: string }>;
};

export async function generateStaticParams() {
  return await searchAllChapterParams();
}

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
      ...(isPublished(chapter) ? { publishedTime: chapter.publishedAt.toISOString() } : {}),
      modifiedTime: chapter.timeline.updatedAt.toISOString(),
    },
    twitter: {
      card: "summary",
      title: chapterTitle,
      description: `${series.title} - ${chapter.title}`,
    },
  };
};

async function ChapterContentSection(props: ContentProps) {
  const params = await props.params;
  const series = await findBySlug(params.slug);

  return (
    <ChapterContent
      slug={slugSchema.parse(params.slug)}
      chapterSlug={slugSchema.parse(params.chapter)}
      seriesChapterIdentifiers={series.chapters}
      renderer={MDXRenderer}
      findChapterBySlug={findChapterBySlug}
      findChaptersByIdentifiers={findPublishedChaptersByIdentifiers}
    />
  );
}

export default function ChapterPage(props: Props) {
  return (
    <Suspense fallback={<ArticleContentSkeleton />}>
      <ChapterContentSection params={props.params} />
    </Suspense>
  );
}
