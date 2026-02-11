import Link from "next/link";
import styles from "./profile.module.css";
import { Profile } from "@shared/domains/user";
import { CircleImage } from "../../image/circle";

export type Props = {
  profile: Profile;
};

export const ProfileCard = (props: Props) => (
  <div className={styles.container}>
    <div className={styles.content}>
      <div className={styles.avatar}>
        <CircleImage alt={props.profile.name} src="/logo/self.png" />
      </div>
      <div className={styles.info}>
        <h3 className={styles.name}>{props.profile.name}</h3>
        <p className={styles.role}>Frontend / Backend Developer</p>
        <p className={styles.description}>{props.profile.bio}</p>
        <Link href="/about" className={styles.link}>
          詳しいプロフィールを見る →
        </Link>
      </div>
    </div>
  </div>
);
