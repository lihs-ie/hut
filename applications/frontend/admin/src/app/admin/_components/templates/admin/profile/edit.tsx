import { ProfileEditForm } from "../../../organisms/admin/profile/edit";
import styles from "./edit.module.css";
import { Profile, UnvalidatedProfile } from "@shared/domains/user";
import { Tag } from "@shared/domains/attributes/tag";

export type Props = {
  getProfile: () => Promise<Profile>;
  persist: (unvalidated: UnvalidatedProfile) => Promise<void>;
  getAllTags: () => Promise<Tag[]>;
};

export const AdminProfileEdit = async (props: Props) => {
  const [profile, tags] = await Promise.all([
    props.getProfile(),
    props.getAllTags(),
  ]);

  return (
    <div className={styles.container}>
      <div className={styles.content}>
        <h1 className={styles.title}>プロフィール設定</h1>

        <ProfileEditForm initial={profile} persist={props.persist} tags={tags} />
      </div>
    </div>
  );
};
