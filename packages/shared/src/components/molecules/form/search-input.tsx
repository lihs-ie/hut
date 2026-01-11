"use client";

import { SearchIcon } from "@shared/components/atoms/icon";
import styles from "./search-input.module.css";

export type Props = {
  placeholder?: string;
  value: string;
  onChange: (value: string) => void;
};

export const SearchInput = (props: Props) => (
  <div className={styles.container}>
    <SearchIcon className={styles.icon} />
    <input
      type="text"
      className={styles.input}
      placeholder={props.placeholder ?? "検索"}
      value={props.value}
      onChange={(event) => props.onChange(event.target.value)}
    />
  </div>
);
