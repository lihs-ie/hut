import { search } from "@shared/actions/tag";
import { TagListIndex } from "../../_components/templates/admin/tag/list";

type Props = {
  searchParams: Promise<{ name?: string }>;
};

export default async function AdminTagsPage(props: Props) {
  const { name } = await props.searchParams;

  return (
    <TagListIndex
      search={search}
      unvalidatedCriteria={{ name: name ?? null }}
    />
  );
}
