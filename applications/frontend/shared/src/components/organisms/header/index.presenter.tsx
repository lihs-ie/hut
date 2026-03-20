import { HeaderLogo } from "@shared/components/atoms/logo/header";
import styles from "./index.module.css";
import { Theme, ThemeToggle } from "@shared/components/molecules/toggle/theme";
import { NavigableLink } from "@shared/components/molecules/link/navigable";
import { SearchIcon } from "@shared/components/atoms/icon/search";
import { LayoutDashboardIcon } from "@shared/components/atoms/icon/layout-dashboard";
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
    <NavigableLink href={Routes.page.top} className={styles.logo}>
      <HeaderLogo />
    </NavigableLink>
    <div className={styles.contents}>
      <NavigableLink href={Routes.page.search} className={styles.searchLink}>
        <SearchIcon />
      </NavigableLink>
      <ThemeToggle value={props.currentTheme} />
      {props.isAdmin && (
        <NavigableLink href="/admin/tags" className={styles.admin} aria-label="管理画面">
          <LayoutDashboardIcon />
        </NavigableLink>
      )}
      {props.isAdmin && <PostMenuDropDown />}
      {props.isAdmin && !!props.logout && (
        <LogoutButton logout={props.logout} />
      )}
    </div>
  </header>
);
