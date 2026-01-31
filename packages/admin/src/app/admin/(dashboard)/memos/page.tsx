import { getAllTags } from "@shared/actions/tag";
import { AdminContentListIndex } from "../../_components/templates/admin/content/list";
import { search, terminate } from "@/actions/memo";
import { ContentType } from "@shared/domains/search-token";
import { Routes } from "@/config/routes";
import z from "zod";

type Props = {
  searchParams: Promise<{
    status?: string;
    freeWord?: string;
    tags?: string[] | string;
  }>;
};

const querySchema = z.object({
  status: z.string().nullish().default(null),
  freeWord: z.string().nullish().default(null),
  tags: z.preprocess((value) => {
    if (typeof value === "string") {
      return value.split(",").map((v) => v.trim());
    }

    return value;
  }, z.array(z.string()).nullish().default(null)),
});

export default async function AdminMemosPage(props: Props) {
  const parameters = await querySchema.parseAsync(props.searchParams);

  return (
    <AdminContentListIndex
      title="メモの管理"
      getAllTags={getAllTags}
      searchContents={search}
      unvalidated={parameters}
      contentType={ContentType.MEMO}
      valuesOf={(content) => ({
        title: content.title,
        status: content.status,
        previewHref: Routes.page.memos.show(content.slug),
        editHref: Routes.page.memos.edit(content.slug),
        updatedAt: content.timeline.updatedAt,
        onTerminate: async () => {
          "use server";

          await terminate(content.identifier);
        },
      })}
    />
  );
}
