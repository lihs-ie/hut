import { Profile } from "@shared/domains/user";
import { FooterPresenter } from "./index.presenter";
import {
  ExternalService,
  ExternalServiceType,
} from "@shared/domains/common/service";

export type Props = {
  getProfile: () => Promise<Profile>;
};

const externalServices = (
  services: ExternalService[],
): Map<ExternalServiceType, string> => {
  const map = new Map<ExternalServiceType, string>();

  services.forEach((service) => {
    map.set(service.type, service.user);
  });

  return map;
};

export const Footer = async (props: Props) => {
  const profile = await props.getProfile();

  return (
    <FooterPresenter
      mailAddress={profile.email}
      externalServices={externalServices(profile.externalServices)}
    />
  );
};
