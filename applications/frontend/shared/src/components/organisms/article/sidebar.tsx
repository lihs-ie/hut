import { Node } from "@shared/components/global/mdx";
import { SidebarPresenter } from "./sidebar.presenter";

export type Props = {
  createTableOfContents: (slug: string) => Promise<Node[]>;
  slug: string;
};

export const Sidebar = async (props: Props) => {
  const root = await props.createTableOfContents(props.slug);

  return <SidebarPresenter root={root} />;
};
