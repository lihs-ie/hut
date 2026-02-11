import { create } from "@/actions/memo";
import { CreateMemoIndex } from "@shared/components/templates/memo/create";

export default async function MemoCreatePage() {
  return <CreateMemoIndex persist={create} />;
}
