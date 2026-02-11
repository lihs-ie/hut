import styles from "./search.module.css";
import { AlertTriangleIcon } from "@shared/components/atoms/icon/alert-triangle";
import { SearchIcon } from "@shared/components/atoms/icon/search";
import { SearchOffIcon } from "@shared/components/atoms/icon/search-off";

type Variant = "initial" | "empty" | "error";

export type Props = {
  variant?: Variant;
  onRetry?: () => void;
};

const variantConfig: Record<
  Variant,
  { title: string; description: string; icon: React.ReactNode }
> = {
  initial: {
    title: "コンテンツを検索",
    description: "キーワードやタグで検索してください",
    icon: <SearchIcon className={styles.searchIcon} />,
  },
  empty: {
    title: "検索結果が見つかりませんでした",
    description: "別のキーワードやフィルターで再度お試しください",
    icon: <SearchOffIcon className={styles.searchIcon} />,
  },
  error: {
    title: "検索中にエラーが発生しました",
    description: "しばらく時間をおいてから再度お試しください",
    icon: <AlertTriangleIcon className={styles.errorIcon} />,
  },
};

export const SearchEmpty = (props: Props) => {
  const config = variantConfig[props.variant ?? "empty"];

  return (
    <div className={styles.container}>
      <div className={styles.icon}>{config.icon}</div>
      <h3 className={styles.title}>{config.title}</h3>
      <p className={styles.description}>{config.description}</p>
      {props.variant === "error" && props.onRetry && (
        <button type="button" className={styles.retry} onClick={props.onRetry}>
          再試行
        </button>
      )}
    </div>
  );
};
