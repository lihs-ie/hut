import { AdminProfileEdit } from "@/app/admin/_components/templates/admin/profile/edit";
import { getProfile, persistProfile } from "@/actions/profile";
import { getAllTags } from "@/actions/tag";

export default function AdminProfileEditPage() {
  return (
    <AdminProfileEdit
      getProfile={getProfile}
      persist={persistProfile}
      getAllTags={getAllTags}
    />
  );
}
