import { VariantButton } from "@shared/components/atoms/button/variant";
import styles from "./button.module.css";

export type Props = {
  logout: () => Promise<void>;
};

export const LogoutButton = async (props: Props) => {
  return (
    <VariantButton
      variant="outline"
      onClick={props.logout}
      className={styles.container}
    >
      ログアウト
    </VariantButton>
  );
};
