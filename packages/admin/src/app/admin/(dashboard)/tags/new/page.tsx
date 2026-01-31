import { find } from "@shared/actions/tag";
import { persist, terminate } from "@/actions/tag";
import { TagEditIndex } from "../../../_components/templates/admin/tag/form";

export default async function TagCreateForm() {
  return <TagEditIndex find={find} persist={persist} terminate={terminate} />;
}
