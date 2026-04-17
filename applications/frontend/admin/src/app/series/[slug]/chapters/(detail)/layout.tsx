import { Suspense } from "react";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug } from "@/actions/series";
import { findChaptersByIdentifiers } from "@/actions/chapter";
import { ChapterToc } from "@shared/components/organisms/series/chapter/toc";
import { ChapterLayoutTemplate } from "@shared/components/templates/series/chapter/layout";
import { ArticleSidebarSkeleton } from "@shared/components/molecules/skeleton";

type Props = {
  params: Promise<{ slug: string }>;
  children: React.ReactNode;
};

type TocProps = {
  params: Promise<{ slug: string }>;
};

async function AdminChapterTocSection(props: TocProps) {
  const params = await props.params;
  const series = await findBySlug(params.slug);
  const slug = slugSchema.parse(params.slug);

  return (
    <ChapterToc
      slug={slug}
      seriesTitle={series.title}
      seriesChapterIdentifiers={series.chapters}
      findChaptersByIdentifiers={findChaptersByIdentifiers}
    />
  );
}

export default function AdminChapterLayout(props: Props) {
  return (
    <ChapterLayoutTemplate
      toc={
        <Suspense fallback={<ArticleSidebarSkeleton />}>
          <AdminChapterTocSection params={props.params} />
        </Suspense>
      }
    >
      {props.children}
    </ChapterLayoutTemplate>
  );
}
