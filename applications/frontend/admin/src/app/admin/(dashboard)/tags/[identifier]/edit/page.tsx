import { find } from "@shared/actions/tag";
import { persist, terminate } from "@/actions/tag";
import { TagEditIndex } from "../../../../_components/templates/admin/tag/form";

type Props = {
  params: Promise<{ identifier: string }>;
};

export default async function TagEditForm(props: Props) {
  const { identifier } = await props.params;

  return (
    <TagEditIndex
      find={find}
      persist={persist}
      identifier={identifier}
      terminate={terminate}
    />
  );
}
