"use client";

import { VariantButton } from "@shared/components/atoms/button/variant";
import styles from "./header.module.css";
import { useRouter } from "next/navigation";
import { Sort } from "@shared/domains/search-token";
import { PlusIcon } from "@shared/components/atoms/icon";
import {
  parseAsArrayOf,
  parseAsString,
  parseAsStringEnum,
  useQueryState,
  UseQueryStateReturn,
} from "nuqs";
import { TextInput } from "@shared/components/atoms/input/text";
import { SimpleSelect } from "@shared/components/atoms/select/simple";
import { PublishStatus } from "@shared/domains/common";
import { TagSelect } from "@shared/components/molecules/select/tag";
import { Tag, TagIdentifier } from "@shared/domains/attributes/tag";

export type Props<Criteria> = {
  title: string;
  newContentPath: string;
  unvalidated: Criteria;
  tagChoices: Tag[];
};

const setArrayParameter =
  <T,>(setter: UseQueryStateReturn<T[] | null, unknown>[1]) =>
  (values: T[] | null, next: T) => {
    if (values?.includes(next)) {
      if (values.length === 1) {
        setter(null);
      } else {
        setter(values.filter((v) => v !== next));
      }
    } else {
      setter([...(values ?? []), next]);
    }
  };

const searchParamParser = {
  title: parseAsString.withOptions({ shallow: false }).withDefault(""),
  sort: parseAsStringEnum(Object.values(Sort))
    .withOptions({ shallow: false })
    .withDefault(Sort.LATEST),
  status: parseAsStringEnum(Object.values(PublishStatus)).withOptions({
    shallow: false,
  }),
  tags: parseAsArrayOf(parseAsString).withDefault([]),
} as const;

export const AdminSearchHeaderPresenter = <Criteria,>(
  props: Props<Criteria>,
) => {
  const router = useRouter();

  const [title, setTitle] = useQueryState("title", searchParamParser.title);
  const [sort, setSort] = useQueryState("sort", searchParamParser.sort);
  const [status, setStatus] = useQueryState("status", searchParamParser.status);
  const [tags, setTags] = useQueryState("tags", searchParamParser.tags);

  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <h2 className={styles.title}>{props.title}</h2>
        <VariantButton
          variant="default"
          onClick={() => router.push(props.newContentPath)}
          className={styles.new}
        >
          <div className={styles.icon}>
            <PlusIcon />
          </div>
          新規作成
        </VariantButton>
      </div>

      <div className={styles.input}>
        <TextInput
          value={title}
          onChange={setTitle}
          placeholder="タイトルを入力"
        />
      </div>
      <div className={styles.selects}>
        <SimpleSelect value={sort} onChange={(value) => setSort(value as Sort)}>
          <option value={""}>すべて</option>
          <option value={PublishStatus.PUBLISHED}>公開済み</option>
          <option value={PublishStatus.DRAFT}>下書き</option>
          <option value={PublishStatus.ARCHIVED}>アーカイブ済み</option>
        </SimpleSelect>
        <SimpleSelect
          value={status || ""}
          onChange={(value) => setStatus(value as PublishStatus)}
        >
          <option value={""}>最新</option>
          <option value={"oldest"}>古い順</option>
        </SimpleSelect>
      </div>
      <div className={styles.tags}>
        <TagSelect
          tags={props.tagChoices}
          onSelect={(tag) => setArrayParameter(setTags)(tags, tag.identifier)}
          selected={(tags as TagIdentifier[] | null) ?? []}
        />
      </div>
    </div>
  );
};
