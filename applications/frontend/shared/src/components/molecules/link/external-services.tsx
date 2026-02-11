import { CircleLink } from "@shared/components/atoms/link/circle";
import styles from "./external-services.module.css";
import { CommonImage } from "@shared/components/atoms/image/common";
import { ExternalService, ExternalServiceType } from "@shared/domains/common/service";

const logos: Record<ExternalServiceType, string> = {
  [ExternalServiceType.GITHUB]: "/logo/github.svg",
  [ExternalServiceType.X]: "/logo/x.svg",
};

const baseURL: Record<ExternalServiceType, string> = {
  [ExternalServiceType.GITHUB]: "https://github.com/",
  [ExternalServiceType.X]: "https://x.com/",
};

export type Props = {
  services: ExternalService[];
};

export const ExternalServiceLinks = (props: Props) => (
  <div className={styles.container}>
    {props.services.map((service) => (
      <CircleLink
        key={service.type}
        href={`${baseURL[service.type as ExternalServiceType]}${service.user}`}
      >
        <div className={`${styles.logo} ${styles[service.type]}`}>
          <CommonImage
            src={logos[service.type as ExternalServiceType]}
            alt={service.type}
          />
        </div>
      </CircleLink>
    ))}
  </div>
);
