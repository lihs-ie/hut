"use client";

import { type ReactNode, useState } from "react";
import { PlusIcon } from "@shared/components/atoms/icon/plus";
import Link from "next/link";
import { SearchInput } from "@shared/components/molecules/form/search-input";
import styles from "./content-list.module.css";

export type SelectOption = {
  value: string;
  label: string;
};

export type Props = {
  title: string;
  createHref: string;
  createLabel?: string;
  searchPlaceholder?: string;
  statusOptions: SelectOption[];
  sortOptions: SelectOption[];
  children: ReactNode;
  onSearch?: (query: string) => void;
  onStatusChange?: (status: string) => void;
  onSortChange?: (sort: string) => void;
};

export const AdminContentList = (props: Props) => {
  const [searchQuery, setSearchQuery] = useState("");
  const [status, setStatus] = useState(props.statusOptions[0]?.value ?? "all");
  const [sort, setSort] = useState(props.sortOptions[0]?.value ?? "latest");

  const handleSearchChange = (value: string) => {
    setSearchQuery(value);
    props.onSearch?.(value);
  };

  const handleStatusChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    const value = event.target.value;
    setStatus(value);
    props.onStatusChange?.(value);
  };

  const handleSortChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    const value = event.target.value;
    setSort(value);
    props.onSortChange?.(value);
  };

  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <h1 className={styles.title}>{props.title}</h1>
        <Link href={props.createHref} className={styles["create-button"]}>
          <PlusIcon className={styles["button-icon"]} />
          {props.createLabel ?? "新規作成"}
        </Link>
      </div>

      <div className={styles.filters}>
        <SearchInput
          placeholder={props.searchPlaceholder ?? "タイトルやトピックで検索"}
          value={searchQuery}
          onChange={handleSearchChange}
        />
        <div className={styles["filter-row"]}>
          <select
            className={styles.select}
            value={status}
            onChange={handleStatusChange}
          >
            {props.statusOptions.map((option) => (
              <option key={option.value} value={option.value}>
                {option.label}
              </option>
            ))}
          </select>
          <select
            className={styles.select}
            value={sort}
            onChange={handleSortChange}
          >
            {props.sortOptions.map((option) => (
              <option key={option.value} value={option.value}>
                {option.label}
              </option>
            ))}
          </select>
        </div>
      </div>

      <div className={styles.list}>{props.children}</div>
    </div>
  );
};
