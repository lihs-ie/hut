import { connection } from "next/server";
import { getProfile } from "@/actions/admin";
import { getAllTags } from "@/actions/tag";
import { AboutIndex } from "@shared/components/templates/about";

/** Reads the public profile through the Reader's runtime data source. */
export default async function Page() {
  await connection();
  return <AboutIndex getProfile={getProfile} getAllTags={getAllTags} now={new Date()} />;
}
