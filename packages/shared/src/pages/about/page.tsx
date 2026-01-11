import { getProfile } from "@shared/actions/admin";
import { AboutIndex } from "@shared/components/templates/about";
import { profile } from "@shared/config/presentation/profile";

export default async function AboutPage() {
  return (
    <AboutIndex
      getProfile={getProfile}
      logoSources={profile.techStack.logoSources}
      now={new Date()}
    />
  );
}
