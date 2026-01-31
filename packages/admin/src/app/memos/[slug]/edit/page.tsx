import { uploadImage } from "@/actions/common";
import { addEntry, edit } from "@/actions/memo";
import { findBySlug, getEntriesBySlug } from "@shared/actions/memo";
import { MDXRenderer } from "@shared/components/global/mdx";
import { MemoEditIndex } from "@shared/components/templates/memo/edit";
import { Slug } from "@shared/domains/common";

export type Props = {
  params: Promise<{ slug: string }>;
};

export default async function MemoEditPage(props: Props) {
  const { slug } = await props.params;

  return (
    <MemoEditIndex
      slug={slug as Slug}
      uploadAction={uploadImage}
      renderer={MDXRenderer}
      getEntries={getEntriesBySlug}
      persistEntry={addEntry}
      findBySlug={findBySlug}
      sidebar={{
        slug: slug as Slug,
        findBySlug,
        edit,
      }}
    />
  );
}
