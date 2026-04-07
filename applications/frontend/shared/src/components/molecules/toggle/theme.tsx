"use client";

import { useSyncExternalStore } from "react";
import { SunIcon } from "@shared/components/atoms/icon/sun";
import styles from "./theme.module.css";
import { MoonIcon } from "@shared/components/atoms/icon/moon";
import { useTheme } from "next-themes";
import { Theme } from "@shared/domains/common/theme";

export { Theme };

const emptySubscribe = () => () => {};
const returnTrue = () => true;
const returnFalse = () => false;

export const ThemeToggle = () => {
  const mounted = useSyncExternalStore(emptySubscribe, returnTrue, returnFalse);
  const { theme, setTheme } = useTheme();
  const isDark = theme === Theme.DARK;

  if (!mounted) {
    return (
      <button className={styles.container} aria-label="Toggle Theme" disabled>
        <SunIcon />
      </button>
    );
  }

  return (
    <button
      className={styles.container}
      onClick={() => setTheme(isDark ? Theme.LIGHT : Theme.DARK)}
      aria-label="Toggle Theme"
    >
      {isDark ? <SunIcon /> : <MoonIcon />}
    </button>
  );
};
