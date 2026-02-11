import { ExternalServiceLinks } from "@shared/components/molecules/link/external-services";
import styles from "./profile.module.css";
import { CircleImage } from "@shared/components/molecules/image/circle";
import { AdminName, Career, MailAddress } from "@shared/domains/user";
import { ExternalService } from "@shared/domains/common/service";
import {
  TechnologyCategory,
  TechnologyStack,
} from "@shared/domains/common/tech";
import { MailIcon } from "@shared/components/atoms/icon/mail";
import Link from "next/link";

export type Props = {
  profile: {
    name: AdminName;
    bio: string;
    externalServices: ExternalService[];
    techStacks: Map<TechnologyCategory, TechnologyStack[]>;
    email: MailAddress;
    careers: Career[];
  };
};

export const ProfilePresenter = (props: Props) => (
  <div className={styles.container}>
    <div className={styles.content}>
      <div className={styles.avatar}>
        <CircleImage src="/logo/self.png" alt="りふす" />
      </div>

      <div className={styles.info}>
        <div className={styles.header}>
          <h1 className={styles.name}>{props.profile.name}</h1>
        </div>
        <p className={styles.bio}>{props.profile.bio}</p>
        <div className={styles.links}>
          <ExternalServiceLinks services={props.profile.externalServices} />
          <Link className={styles.email} href={`mailto:${props.profile.email}`}>
            <MailIcon />
          </Link>
        </div>
      </div>
    </div>
  </div>
);
