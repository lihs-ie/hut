import Link from "next/link";
import { ArrowLeftIcon } from "@shared/components/atoms/icon/arrow-left";
import { Tag, UnvalidatedTag } from "@shared/domains/attributes/tag";
import styles from "./form.module.css";
import { Routes } from "@/config/routes";
import { TagEditForm } from "../../../organisms/admin/tag/form";
import { SimpleCard } from "@shared/components/atoms/card/simple";

export type Props = {
  identifier?: string;
  find: (identifier: string) => Promise<Tag>;
  persist: (unvalidated: UnvalidatedTag) => Promise<void>;
  terminate: (identifier: string) => Promise<void>;
};

export const TagEditIndex = async (props: Props) => {
  const initial = props.identifier
    ? await props.find(props.identifier)
    : undefined;

  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <Link href={Routes.admin.tag.list} className={styles["back-link"]}>
          <ArrowLeftIcon className={styles.icon} />
          タグ一覧に戻る
        </Link>
        <h1 className={styles.title}>
          {!initial ? "新規タグ作成" : "タグ編集"}
        </h1>
      </div>

      <SimpleCard>
        <TagEditForm
          initial={initial}
          persist={props.persist}
          terminate={props.terminate}
        />
      </SimpleCard>
    </div>
  );
};
