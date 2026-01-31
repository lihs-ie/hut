import { AdminSidebar, NavItem } from "../_components/organisms/admin/sidebar";
import { Routes } from "@/config/routes";
import styles from "./layout.module.css";
import {
  // [初期リリース対象外] BookOpenIcon,
  MessageSquareIcon,
  TagIcon,
  UserIcon,
  ShieldIcon,
  FileTextIcon,
} from "@shared/components/atoms/icon";

const navItems: NavItem[] = [
  {
    href: Routes.admin.articles.list,
    label: "記事の管理",
    icon: <FileTextIcon />,
  },
  {
    href: Routes.admin.memos.list,
    label: "メモの管理",
    icon: <MessageSquareIcon />,
  },
  // [初期リリース対象外] {
  //   href: Routes.admin.series.list,
  //   label: "シリーズの管理",
  //   icon: <BookOpenIcon />,
  // },
  { href: Routes.admin.tag.list, label: "タグ管理", icon: <TagIcon /> },
  {
    href: Routes.admin.profile.edit,
    label: "プロフィール",
    icon: <UserIcon />,
  },
  {
    href: Routes.admin.privacy.edit,
    label: "プライバシーポリシー",
    icon: <ShieldIcon />,
  },
];

export default function AdminLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <div className={styles.container}>
      <AdminSidebar items={navItems} />
      <div className={styles.content}>{children}</div>
    </div>
  );
}
