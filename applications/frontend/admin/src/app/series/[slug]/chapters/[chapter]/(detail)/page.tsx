import { Suspense } from "react";
import { MDXRenderer } from "@shared/components/global/mdx";
import { ArticleContentSkeleton } from "@shared/components/molecules/skeleton";
import { ChapterContent } from "@shared/components/organisms/series/chapter/content";
import { findChapterBySlug, findChaptersByIdentifiers } from "@/actions/chapter";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug } from "@/actions/series";

type Props = {
  params: Promise<{ slug: string; chapter: string }>;
};

type ContentProps = {
  params: Promise<{ slug: string; chapter: string }>;
};

async function AdminChapterContentSection(props: ContentProps) {
  const params = await props.params;
  const series = await findBySlug(params.slug);

  return (
    <ChapterContent
      slug={slugSchema.parse(params.slug)}
      chapterSlug={slugSchema.parse(params.chapter)}
      seriesChapterIdentifiers={series.chapters}
      renderer={MDXRenderer}
      findChapterBySlug={findChapterBySlug}
      findChaptersByIdentifiers={findChaptersByIdentifiers}
    />
  );
}

export default function AdminChapterPage(props: Props) {
  return (
    <Suspense fallback={<ArticleContentSkeleton />}>
      <AdminChapterContentSection params={props.params} />
    </Suspense>
  );
}
