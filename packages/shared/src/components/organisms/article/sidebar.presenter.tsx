import { Node } from "@shared/components/global/mdx";
import styles from "./sidebar.module.css";
import { TableOfContents } from "@shared/components/molecules/list/table-of-contents";

export type Props = {
  root: Node[];
};

export const SidebarPresenter = (props: Props) => (
  <div className={styles.container}>
    <TableOfContents root={props.root} />
  </div>
);
