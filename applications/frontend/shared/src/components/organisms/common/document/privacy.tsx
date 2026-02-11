import { PrivacyPolicy } from "@shared/domains/document";
import { PrivacySectionsPresenter } from "./privacy.presenter";

export type Props = {
  getPrivacy: () => Promise<PrivacyPolicy>;
};
export const revalidate = false;

export const PrivacySections = async (props: Props) => {
  const privacy = await props.getPrivacy();

  return <PrivacySectionsPresenter privacy={privacy} />;
};
