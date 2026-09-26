import { connection } from "next/server";
import { getPrivacyPolicy } from "@/actions/document";
import { PrivacyIndex } from "@shared/components/templates/legal/privacy";

/** Defers the privacy document read until a Reader request begins. */
export default async function Page() {
  await connection();
  return <PrivacyIndex getPrivacy={getPrivacyPolicy} />;
}
