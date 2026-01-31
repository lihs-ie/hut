import styles from "./tag.module.css";
import { TagBadge, Props as TagBadgeProps } from "../../../molecules/badge/tag";
import Link from "next/link";
import { Routes } from "@shared/config/presentation/route";

export type Props = {
  tags: TagBadgeProps[];
};

export const TagBadgeListPresenter = (props: Props) => (
  <div className={styles.container}>
    {props.tags.map((tag) => (
      <Link
        className={styles.badge}
        key={tag.name}
        href={`${Routes.page.search}?tags=${encodeURIComponent(tag.name)}`}
      >
        <TagBadge {...tag} />
      </Link>
    ))}
  </div>
);
