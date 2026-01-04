import { HeaderLogo } from "@/components/atoms/logo/header";
import styles from "./index.module.css";
import { ThemeToggle } from "@/components/atoms/toggle/theme";
import { currentTheme, toggleTheme } from "@/app/actions/theme";
import { isAdmin } from "@/app/actions/admin";
import { SimpleButton } from "@/components/atoms/button/simple";

export const Header = async () => {
  const theme = await currentTheme();
  const admin = await isAdmin();

  return (
    <header className={styles.container}>
      <HeaderLogo />
      <div className={styles.contents}>
        <ThemeToggle value={theme} onToggle={toggleTheme} />
        <button>検索</button>
        {admin && <SimpleButton>投稿する</SimpleButton>}
      </div>
    </header>
  );
};
