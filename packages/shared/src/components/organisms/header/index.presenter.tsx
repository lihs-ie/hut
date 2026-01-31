import { HeaderLogo } from "@shared/components/atoms/logo/header";
import styles from "./index.module.css";
import { Theme, ThemeToggle } from "@shared/components/molecules/toggle/theme";
import Link from "next/link";
import { SearchIcon } from "@shared/components/atoms/icon/search";
import { LayoutDashboardIcon } from "@shared/components/atoms/icon";
import { Routes } from "@shared/config/presentation/route";
import { PostMenuDropDown } from "@shared/components/molecules/drop-down/post-menu";
import { LogoutButton } from "@shared/components/molecules/button/logout";

export type Props = {
  currentTheme: Theme;
  isAdmin: boolean;
  logout?: () => Promise<void>;
};

export const HeaderPresenter = (props: Props) => (
  <header className={styles.container}>
    <Link href={Routes.page.top} className={styles.logo}>
      <HeaderLogo />
    </Link>
    <div className={styles.contents}>
      <Link href={Routes.page.search} className={styles.searchLink}>
        <SearchIcon />
      </Link>
      <ThemeToggle value={props.currentTheme} />
      {props.isAdmin && (
        <Link href="/admin/tags" className={styles.admin} aria-label="管理画面">
          <LayoutDashboardIcon />
        </Link>
      )}
      {props.isAdmin && <PostMenuDropDown />}
      {props.isAdmin && !!props.logout && (
        <LogoutButton logout={props.logout} />
      )}
    </div>
  </header>
);
