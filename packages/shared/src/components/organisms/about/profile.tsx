import { ProfilePresenter, Props as PresenterProps } from "./profile.presenter";

export type Props = {
  getProfile: () => Promise<PresenterProps["profile"]>;
};

export const Profile = async (props: Props) => {
  const profile = await props.getProfile();

  return <ProfilePresenter profile={profile} />;
};
