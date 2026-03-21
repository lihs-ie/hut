import { findAllTags } from "@shared/actions/tag";
import { findChaptersByIdentifiers } from "@shared/actions/chapter";
import { SeriesIndex } from "@shared/components/templates/series";
import { slugSchema } from "@shared/domains/common/slug";
import { findBySlug } from "@shared/actions/series";
import type { Metadata } from "next";

type Props = {
  params: Promise<{ slug: string }>;
};

export const generateMetadata = async (props: Props): Promise<Metadata> => {
  const { slug } = await props.params;
  const series = await findBySlug(slug);
  const tags = await findAllTags(series.tags);

  return {
    title: series.title,
    description: series.description ?? `${series.title}のシリーズページです`,
    keywords: tags.map((tag) => tag.name),
    authors: [{ name: "lihs" }],
    openGraph: {
      type: "article",
      title: series.title,
      description: series.description ?? undefined,
      publishedTime: series.timeline.createdAt.toISOString(),
      modifiedTime: series.timeline.updatedAt.toISOString(),
      tags: tags.map((tag) => tag.name),
      ...(series.cover ? { images: [{ url: series.cover }] } : {}),
    },
    twitter: {
      card: series.cover ? "summary_large_image" : "summary",
      title: series.title,
      description: series.description ?? undefined,
    },
  };
};

export default async function SeriesDetailPage(props: Props) {
  const params = await props.params;

  return (
    <SeriesIndex
      slug={slugSchema.parse(params.slug)}
      findBySlug={findBySlug}
      findChaptersByIdentifiers={findChaptersByIdentifiers}
      findAllTags={findAllTags}
    />
  );
}
