"use client";

import { Tag } from "@shared/domains/attributes/tag";
import { TagCard } from "@shared/components/molecules/list/card/admin/tag";
import styles from "./list.module.css";
import { parseAsString, useQueryState } from "nuqs";
import { TextInput } from "@shared/components/atoms/input/text";
import { Routes } from "@/config/routes";

export type Props = {
  tags: Tag[];
};

const searchParamParsers = {
  name: parseAsString.withDefault("").withOptions({ shallow: false }),
} as const;

export const TagListPresenter = (props: Props) => {
  const [name, setName] = useQueryState("name", searchParamParsers.name);

  return (
    <div className={styles.container}>
      <div className={styles["search-bar"]}>
        <TextInput value={name} onChange={setName} placeholder="タグ名で検索" />
      </div>

      {props.tags.length > 0 ? (
        <div className={styles.grid}>
          {props.tags.map((tag) => (
            <TagCard
              key={tag.identifier}
              logo={tag.logo}
              name={tag.name}
              createdAt={tag.timeline.createdAt}
              href={Routes.admin.tag.edit(tag.identifier)}
            />
          ))}
        </div>
      ) : (
        <div className={styles.empty}>
          {name
            ? "検索結果がありません"
            : "タグがありません。新規作成してください。"}
        </div>
      )}
    </div>
  );
};
