import { Suspense } from "react";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug } from "@/actions/series";
import { findPublishedChaptersByIdentifiers } from "@/actions/chapter";
import { ChapterToc } from "@shared/components/organisms/series/chapter/toc";
import { TocDrawer } from "@shared/components/organisms/series/chapter/toc-drawer";
import { ChapterLayoutTemplate } from "@shared/components/templates/series/chapter/layout";
import { ArticleSidebarSkeleton } from "@shared/components/molecules/skeleton";

export const revalidate = 3600;

type Props = {
  params: Promise<{ slug: string }>;
  children: React.ReactNode;
};

type TocProps = {
  params: Promise<{ slug: string }>;
};

async function ChapterTocSection(props: TocProps) {
  const params = await props.params;
  const series = await findBySlug(params.slug);
  const slug = slugSchema.parse(params.slug);

  return (
    <ChapterToc
      slug={slug}
      seriesTitle={series.title}
      seriesChapterIdentifiers={series.chapters}
      findChaptersByIdentifiers={findPublishedChaptersByIdentifiers}
    />
  );
}

export default function ChapterLayout(props: Props) {
  return (
    <ChapterLayoutTemplate
      toc={
        <TocDrawer>
          <Suspense fallback={<ArticleSidebarSkeleton />}>
            <ChapterTocSection params={props.params} />
          </Suspense>
        </TocDrawer>
      }
    >
      {props.children}
    </ChapterLayoutTemplate>
  );
}
