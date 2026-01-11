import { Suspense } from "react";
import styles from "./index.module.css";
import { Profile as ProfileComponent } from "@shared/components/organisms/about/profile";
import { Profile } from "@shared/domains/user";
import { TechStackSection } from "@shared/components/organisms/about/tech-stack";
import { TechnologyKind } from "@shared/domains/common/tech";
import { CareerSection } from "@shared/components/organisms/about/career";
import {
  ProfileSkeleton,
  TechStackSkeleton,
  CareerSkeleton,
} from "@shared/components/molecules/skeleton";

export type Props = {
  getProfile: () => Promise<Profile>;
  logoSources: Record<TechnologyKind, string>;
  now: Date;
};

export const AboutIndex = async (props: Props) => (
  <div className={styles.container}>
    <h2 className={styles.title}>About Me</h2>
    <Suspense fallback={<ProfileSkeleton />}>
      <ProfileComponent getProfile={props.getProfile} />
    </Suspense>
    <Suspense fallback={<TechStackSkeleton />}>
      <TechStackSection
        getProfile={props.getProfile}
        logoSources={props.logoSources}
        now={props.now}
      />
    </Suspense>
    <Suspense fallback={<CareerSkeleton />}>
      <CareerSection getProfile={props.getProfile} />
    </Suspense>
  </div>
);
