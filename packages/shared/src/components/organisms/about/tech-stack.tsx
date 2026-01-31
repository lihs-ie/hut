import { TechStackList } from "@shared/components/molecules/list/tech-stack";
import { TechnologyKind } from "@shared/domains/common/tech";
import { Profile } from "@shared/domains/user";

export type Props = {
  getProfile: () => Promise<Profile>;
  logoSources: Record<TechnologyKind, string>;
  now: Date;
};

export const TechStackSection = async (props: Props) => {
  const profile = await props.getProfile();

  return (
    <TechStackList
      techStacks={profile.techStacks}
      now={props.now}
      logoSources={props.logoSources}
    />
  );
};
