import { PrivacyEdit } from "../../../_components/templates/admin/privacy/edit";
import { getPrivacyPolicy } from "@/actions/document";
import { persistPrivacyPolicy } from "@/actions/document";

export default function AdminPrivacyEditPage() {
  return (
    <PrivacyEdit
      getPrivacyPolicy={getPrivacyPolicy}
      persist={persistPrivacyPolicy}
    />
  );
}
