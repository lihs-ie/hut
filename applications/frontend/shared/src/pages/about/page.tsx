import { getProfile } from "@shared/actions/admin";
import { getAllTags } from "@shared/actions/tag";
import { AboutIndex } from "@shared/components/templates/about";

export default async function AboutPage() {
  return (
    <AboutIndex
      getProfile={getProfile}
      getAllTags={getAllTags}
      now={new Date()}
    />
  );
}
