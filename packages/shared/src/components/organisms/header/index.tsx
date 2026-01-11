import { currentTheme } from "@shared/actions/theme";
import { HeaderPresenter } from "./index.presenter";

export type Props = {
  isAdmin?: () => Promise<boolean>;
  logout?: () => Promise<void>;
};

export const Header = async (props: Props) => {
  const theme = await currentTheme();
  const isAdmin = props.isAdmin ? await props.isAdmin() : false;

  return (
    <HeaderPresenter
      currentTheme={theme}
      isAdmin={isAdmin}
      logout={props.logout}
    />
  );
};
