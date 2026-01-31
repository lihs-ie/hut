import { CareerList } from "@shared/components/molecules/list/career";
import { Profile } from "@shared/domains/user";

export type Props = {
  getProfile: () => Promise<Profile>;
};

export const CareerSection = async (props: Props) => {
  const profile = await props.getProfile();

  return <CareerList careers={profile.careers} />;
};
