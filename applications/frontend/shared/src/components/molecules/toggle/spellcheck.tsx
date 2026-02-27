"use client";

import { SimpleSwitch } from "@shared/components/atoms/toggle/simple";
import styles from "./spellcheck.module.css";

type Props = {
  checked: boolean;
  onChange: (checked: boolean) => void;
  label?: string;
};

export const SpellcheckToggle = (props: Props) => {
  return (
    <div className={styles.container}>
      <span className={styles.label}>
        {props.label ?? "スペルチェック"}
      </span>
      <SimpleSwitch
        checked={props.checked}
        onChange={props.onChange}
        aria-label="スペルチェック"
      />
    </div>
  );
};
