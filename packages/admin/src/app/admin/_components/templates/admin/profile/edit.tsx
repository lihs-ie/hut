import { ProfileEditForm } from "../../../organisms/admin/profile/edit";
import styles from "./edit.module.css";
import { Profile, UnvalidatedProfile } from "@shared/domains/user";

export type Props = {
  getProfile: () => Promise<Profile>;
  persist: (unvalidated: UnvalidatedProfile) => Promise<void>;
};

export const AdminProfileEdit = async (props: Props) => {
  const profile = await props.getProfile();

  return (
    <div className={styles.container}>
      <div className={styles.content}>
        <h1 className={styles.title}>プロフィール設定</h1>

        <ProfileEditForm initial={profile} persist={props.persist} />
      </div>
    </div>
  );
};
