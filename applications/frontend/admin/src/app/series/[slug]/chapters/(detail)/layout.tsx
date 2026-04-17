import { Suspense } from "react";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug } from "@/actions/series";
import { findChaptersByIdentifiers } from "@/actions/chapter";
import { ChapterTOC } from "@shared/components/organisms/series/chapter/toc";
import { TOCDrawer } from "@shared/components/organisms/series/chapter/toc/drawer";
import { ChapterLayoutTemplate } from "@shared/components/templates/series/chapter/layout";
import { ArticleSidebarSkeleton } from "@shared/components/molecules/skeleton";

type Props = {
  params: Promise<{ slug: string }>;
  children: React.ReactNode;
};

type TOCProps = {
  params: Promise<{ slug: string }>;
};

async function AdminChapterTOCSection(props: TOCProps) {
  const params = await props.params;
  const series = await findBySlug(params.slug);
  const slug = slugSchema.parse(params.slug);

  return (
    <ChapterTOC
      slug={slug}
      seriesTitle={series.title}
      chapters={series.chapters}
      findChaptersByIdentifiers={findChaptersByIdentifiers}
    />
  );
}

export default function AdminChapterLayout(props: Props) {
  return (
    <ChapterLayoutTemplate
      toc={
        <TOCDrawer>
          <Suspense fallback={<ArticleSidebarSkeleton />}>
            <AdminChapterTOCSection params={props.params} />
          </Suspense>
        </TOCDrawer>
      }
    >
      {props.children}
    </ChapterLayoutTemplate>
  );
}
