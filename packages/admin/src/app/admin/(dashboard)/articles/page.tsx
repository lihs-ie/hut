import { getAllTags } from "@shared/actions/tag";
import { AdminContentListIndex } from "../../_components/templates/admin/content/list";
import { search, terminate } from "@/actions/article";
import { ContentType } from "@shared/domains/search-token";
import { Routes } from "@/config/routes";
import z from "zod";

type Props = {
  searchParams: Promise<{
    slug?: string;
    status?: string;
    freeWord?: string;
    tags?: string[] | string;
  }>;
};

const querySchema = z.object({
  slug: z.string().nullish().default(null),
  status: z.string().nullish().default(null),
  freeWord: z.string().nullish().default(null),
  tags: z.preprocess((value) => {
    if (typeof value === "string") {
      return value.split(",").map((v) => v.trim());
    }

    return value;
  }, z.array(z.string()).nullish().default(null)),
});

export default async function AdminArticlesPage(props: Props) {
  const parameters = await querySchema.parseAsync(props.searchParams);

  return (
    <AdminContentListIndex
      title="記事の管理"
      getAllTags={getAllTags}
      searchContents={search}
      unvalidated={parameters}
      contentType={ContentType.ARTICLE}
      valuesOf={(content) => ({
        title: content.title,
        status: content.status,
        previewHref: Routes.page.articles.show(content.slug),
        editHref: Routes.page.articles.edit(content.slug),
        updatedAt: content.timeline.updatedAt,
        onTerminate: async () => {
          "use server";
          await terminate(content.identifier);
        },
      })}
    />
  );
}
