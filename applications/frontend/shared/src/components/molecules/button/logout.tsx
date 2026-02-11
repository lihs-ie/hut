"use client";

import styles from "./logout.module.css";
import { LogoutIcon } from "@shared/components/atoms/icon/logout";

export type Props = {
  logout: () => Promise<void>;
};

export const LogoutButton = (props: Props) => (
  <form className={styles.container} action={props.logout}>
    <button type="submit" className={styles.button}>
      <LogoutIcon className={styles.icon} />
      <span className={styles.label}>ログアウト</span>
    </button>
  </form>
);
