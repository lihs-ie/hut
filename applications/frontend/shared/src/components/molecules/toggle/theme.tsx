"use client";

import { SunIcon } from "@shared/components/atoms/icon/sun";
import styles from "./theme.module.css";
import { MoonIcon } from "@shared/components/atoms/icon/moon";
import { toggleTheme } from "@shared/actions/theme";
import { Theme } from "@shared/domains/common/theme";

export { Theme };

export type Props = {
  value: Theme;
};

export const ThemeToggle = (props: Props) => (
  <button
    className={`${styles.container} ${styles[props.value]}`}
    onClick={() => toggleTheme()}
    aria-label="Toggle Theme"
  >
    {props.value === Theme.LIGHT ? <SunIcon /> : <MoonIcon />}
  </button>
);
