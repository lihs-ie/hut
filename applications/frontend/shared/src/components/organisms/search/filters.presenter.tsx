"use client";

import { useState } from "react";
import { SearchIcon } from "@shared/components/atoms/icon/search";
import { FilterIcon } from "@shared/components/atoms/icon/filter";
import { XIcon } from "@shared/components/atoms/icon/cross";
import { FileTextIcon } from "@shared/components/atoms/icon/file-text";
import { MessageSquareIcon } from "@shared/components/atoms/icon/message";
import { ChevronDownIcon } from "@shared/components/atoms/icon/chevron-down";
import { TextInput } from "@shared/components/atoms/input/text";
import { VariantButton } from "@shared/components/atoms/button/variant";
import { Tag } from "@shared/domains/attributes/tag";
import styles from "./filters.module.css";
import {
  parseAsArrayOf,
  parseAsString,
  parseAsStringEnum,
  useQueryState,
  UseQueryStateReturn,
} from "nuqs";
import { ContentType, Sort } from "@shared/domains/search-token";
import { useRouter } from "next/navigation";
import { TagSelect } from "../../molecules/select/tag";

export type Props = {
  tags: Tag[];
};

const searchParamsParsers = {
  freeWord: parseAsString.withDefault("").withOptions({ shallow: false }),
  tags: parseAsArrayOf(parseAsString)
    .withDefault([])
    .withOptions({ shallow: false }),
  type: parseAsStringEnum<ContentType>(Object.values(ContentType)).withOptions({
    shallow: false,
  }),
  sortBy: parseAsStringEnum<Sort>(Object.values(Sort))
    .withDefault(Sort.LATEST)
    .withOptions({ shallow: false }),
};

const hasActiveFilters = (
  tagNames: string[] | null,
  type: ContentType | null,
  sortBy: Sort | null,
) => {
  if (tagNames && tagNames.length > 0) {
    return true;
  }

  if (type != null) {
    return true;
  }

  if (sortBy && sortBy !== Sort.LATEST) {
    return true;
  }

  return false;
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

export const SearchFilterPresenter = (props: Props) => {
  const router = useRouter();

  const [freeWord, setFreeWord] = useQueryState(
    "freeWord",
    searchParamsParsers.freeWord,
  );
  const [tagNames, setTagNames] = useQueryState(
    "tags",
    searchParamsParsers.tags,
  );
  const [type, setType] = useQueryState("type", searchParamsParsers.type);
  const [sortBy, setSortBy] = useQueryState(
    "sortBy",
    searchParamsParsers.sortBy,
  );

  const [isFilterExpanded, setIsFilterExpanded] = useState(() => {
    if (freeWord !== null || freeWord !== "") {
      return true;
    }

    if (tagNames !== null && tagNames.length > 0) {
      return true;
    }

    if (type !== null) {
      return true;
    }

    if (sortBy !== null && sortBy !== Sort.LATEST) {
      return true;
    }

    return false;
  });

  const selectedTagIdentifiers = props.tags
    .filter((tag) => (tagNames ?? []).includes(tag.name))
    .map((tag) => tag.identifier);

  const activeFilter = hasActiveFilters(tagNames, type, sortBy);

  const clear = () => {
    setFreeWord("");
    setTagNames(null);
    setType(null);
    setSortBy(Sort.LATEST);
  };

  return (
    <div className={styles.container}>
      <div className={styles["search-box"]}>
        <div className={styles["search-input-wrapper"]}>
          <SearchIcon className={styles["search-icon"]} />
          <TextInput
            type="search"
            placeholder="キーワードで検索..."
            value={freeWord}
            onChange={setFreeWord}
            className={styles["search-input"]}
            autoFocus
          />
        </div>
      </div>

      <div className={styles["filter-panel"]}>
        <div className={styles["filter-header"]}>
          <button
            className={styles["filter-toggle"]}
            onClick={() => setIsFilterExpanded(!isFilterExpanded)}
            aria-expanded={isFilterExpanded}
          >
            <div className={styles.icon}>
              <FilterIcon className={styles["filter-icon"]} />
            </div>
            <span className={styles["filter-text"]}>フィルター</span>
            <div className={styles.icon}>
              <ChevronDownIcon
                className={`${styles.chevron} ${
                  isFilterExpanded ? styles["chevron-expanded"] : ""
                  }`}
                  />
            </div>
          </button>

          {activeFilter && (
            <VariantButton
              variant="ghost"
              size="sm"
              onClick={() => {
                clear();
                router.refresh();
              }}
              className={styles["clear-button"]}
            >
              <XIcon className={styles["clear-icon"]} />
              クリア
            </VariantButton>
          )}
        </div>

        <div
          className={`${styles["filter-content"]} ${
            isFilterExpanded ? styles["filter-content-expanded"] : ""
          }`}
        >
          <div className={styles["filter-section"]}>
            <label className={styles["filter-label"]}>種別</label>
            <div className={styles["button-group"]}>
              <VariantButton
                variant={type == null ? "default" : "outline"}
                size="sm"
                onClick={() => {
                  setType(null);
                  router.refresh();
                }}
                className={styles["filter-button"]}
              >
                すべて
              </VariantButton>
              <VariantButton
                variant={type === ContentType.ARTICLE ? "default" : "outline"}
                size="sm"
                onClick={() => setType(ContentType.ARTICLE)}
                className={styles["filter-button"]}
              >
                <FileTextIcon className={styles["button-icon"]} />
                記事
              </VariantButton>
              <VariantButton
                variant={type === ContentType.MEMO ? "default" : "outline"}
                size="sm"
                onClick={() => setType(ContentType.MEMO)}
                className={styles["filter-button"]}
              >
                <MessageSquareIcon className={styles["button-icon"]} />
                メモ
              </VariantButton>
              {/* <VariantButton
                variant={
                  types?.includes(ContentType.SERIES) ? "default" : "outline"
                }
                size="sm"
                onClick={() =>
                  setArrayParameter(setTypes)(types, ContentType.SERIES)
                }
                className={styles["filter-button"]}
              >
                <BookOpenIcon className={styles["button-icon"]} />本
              </VariantButton> */}
            </div>
          </div>

          <div className={styles["filter-section"]}>
            <label className={styles["filter-label"]}>並び替え</label>
            <div className={styles["button-group"]}>
              <VariantButton
                variant={sortBy === Sort.LATEST ? "default" : "outline"}
                size="sm"
                onClick={() => setSortBy(Sort.LATEST)}
                className={styles["filter-button"]}
              >
                最新
              </VariantButton>
              <VariantButton
                variant={sortBy === Sort.OLDEST ? "default" : "outline"}
                size="sm"
                onClick={() => setSortBy(Sort.OLDEST)}
                className={styles["filter-button"]}
              >
                古い順
              </VariantButton>
            </div>
          </div>

          <div className={styles["filter-section"]}>
            <label className={styles["filter-label"]}>タグ</label>
            <div className={styles["tag-section-content"]}>
              <TagSelect
                tags={props.tags}
                selected={selectedTagIdentifiers}
                onSelect={(tag) =>
                  setArrayParameter(setTagNames)(tagNames, tag.name)
                }
              />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};
