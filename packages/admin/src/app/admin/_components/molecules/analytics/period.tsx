"use client";

import { parseAsStringEnum, useQueryState } from "nuqs";
import styles from "./period.module.css";

const periods = {
  "7d": "過去7日",
  "30d": "過去30日",
  "90d": "過去90日",
  all: "全期間",
} as const;

type Period = keyof typeof periods;

const periodParser = parseAsStringEnum<Period>(
  Object.keys(periods) as Array<Period>
)
  .withOptions({ shallow: false })
  .withDefault("30d");

export const AnalyticsPeriodSelector = () => {
  const [period, setPeriod] = useQueryState("period", periodParser);

  return (
    <nav className={styles.container} aria-label="期間選択">
      {(Object.entries(periods) as Array<[Period, string]>).map(
        ([key, label]) => {
          return (
            <button
              key={key}
              type="button"
              className={`${styles.button} ${period === key ? styles.active : ""}`}
              onClick={() => setPeriod(key)}
              aria-pressed={period === key}
            >
              {label}
            </button>
          );
        }
      )}
    </nav>
  );
};
