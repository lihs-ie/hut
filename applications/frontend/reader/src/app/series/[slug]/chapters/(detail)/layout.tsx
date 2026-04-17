import { Suspense } from "react";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug } from "@/actions/series";
import { findPublishedChaptersByIdentifiers } from "@/actions/chapter";
import { ChapterTOC } from "@shared/components/organisms/series/chapter/toc";
import { TOCDrawer } from "@shared/components/organisms/series/chapter/toc/drawer";
import { ChapterLayoutTemplate } from "@shared/components/templates/series/chapter/layout";
import { ArticleSidebarSkeleton } from "@shared/components/molecules/skeleton";

export const revalidate = 3600;

type Props = {
  params: Promise<{ slug: string }>;
  children: React.ReactNode;
};

type TOCProps = {
  params: Promise<{ slug: string }>;
};

async function ChapterTOCSection(props: TOCProps) {
  const params = await props.params;
  const series = await findBySlug(params.slug);
  const slug = slugSchema.parse(params.slug);

  return (
    <ChapterTOC
      slug={slug}
      seriesTitle={series.title}
      chapters={series.chapters}
      findChaptersByIdentifiers={findPublishedChaptersByIdentifiers}
    />
  );
}

export default function ChapterLayout(props: Props) {
  return (
    <ChapterLayoutTemplate
      toc={
        <TOCDrawer>
          <Suspense fallback={<ArticleSidebarSkeleton />}>
            <ChapterTOCSection params={props.params} />
          </Suspense>
        </TOCDrawer>
      }
    >
      {props.children}
    </ChapterLayoutTemplate>
  );
}
