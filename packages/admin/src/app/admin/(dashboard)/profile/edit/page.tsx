import { AdminProfileEdit } from "@/app/admin/_components/templates/admin/profile/edit";
import { getProfile } from "@shared/actions/admin";
import { persistProfile } from "@/actions/profile";

export default function AdminProfileEditPage() {
  return <AdminProfileEdit getProfile={getProfile} persist={persistProfile} />;
}
