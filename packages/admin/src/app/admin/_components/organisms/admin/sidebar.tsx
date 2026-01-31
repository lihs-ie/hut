import { type ReactNode } from "react";
import { AdminSidebarPresenter } from "./sidebar.presenter";

export type NavItem = {
  href: string;
  icon: ReactNode;
  label: string;
};

export type Props = {
  items: NavItem[];
};

export const AdminSidebar = (props: Props) => {
  return <AdminSidebarPresenter items={props.items} />;
};
