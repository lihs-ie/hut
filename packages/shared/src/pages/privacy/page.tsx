import { getPrivacyPolicy } from "@shared/actions/document";
import { PrivacyIndex } from "@shared/components/templates/legal/privacy";

export default async function PrivacyPolicyPage() {
  return <PrivacyIndex getPrivacy={getPrivacyPolicy} />;
}
