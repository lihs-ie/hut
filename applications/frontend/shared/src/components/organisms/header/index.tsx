import { HeaderPresenter } from "./index.presenter";

export type Props = {
  isAdmin?: () => Promise<boolean>;
  logout?: () => Promise<void>;
};

export const Header = async (props: Props) => {
  const isAdmin = props.isAdmin ? await props.isAdmin() : false;

  return (
    <HeaderPresenter
      isAdmin={isAdmin}
      logout={props.logout}
    />
  );
};
