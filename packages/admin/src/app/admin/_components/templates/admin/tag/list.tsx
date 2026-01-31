import Link from "next/link";
import { PlusIcon } from "@shared/components/atoms/icon";
import { Tag, UnvalidatedCriteria } from "@shared/domains/attributes/tag";
import styles from "./list.module.css";
import { TagList } from "../../../organisms/admin/tag/list";
import { TagListSkeleton } from "../../../organisms/admin/tag/list.skeleton";
import { Routes } from "@/config/routes";
import { Suspense } from "react";

export type Props = {
  search: (unvalidated: UnvalidatedCriteria) => Promise<Tag[]>;
  unvalidatedCriteria: UnvalidatedCriteria;
};

export const TagListIndex = (props: Props) => (
  <div className={styles.container}>
    <div className={styles.header}>
      <h1 className={styles.title}>タグ管理</h1>
      <Link href={Routes.admin.tag.new} className={styles.new}>
        <PlusIcon className={styles.icon} />
        新規作成
      </Link>
    </div>

    <Suspense fallback={<TagListSkeleton />}>
      <TagList search={props.search} unvalidated={props.unvalidatedCriteria} />
    </Suspense>
  </div>
);
