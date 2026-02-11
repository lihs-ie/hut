import { StatsCard } from "@shared/components/atoms/card/stats";
import {
  RecentList,
  type RecentItem,
} from "@shared/components/molecules/list/recent";
import {
  ActivityList,
  type ActivityItem,
} from "@shared/components/molecules/list/activity";
import styles from "./index.module.css";

export type Stats = {
  articles: number;
  memos: number;
  drafts: number;
  monthlyPosts: number;
};

export type Props = {
  stats: Stats;
  recentItems: RecentItem[];
  activities: ActivityItem[];
};

export const DashboardPresenter = (props: Props) => (
  <div className={styles.container}>
    <div className={styles.header}>
      <h1 className={styles.title}>ダッシュボード</h1>
      <p className={styles.description}>コンテンツの概要と最近のアクティビティ</p>
    </div>

    <div className={styles["stats-grid"]}>
      <StatsCard
        title="記事数"
        value={props.stats.articles}
        trend={{ value: 12, label: "先月比" }}
      />
      <StatsCard
        title="メモ数"
        value={props.stats.memos}
        trend={{ value: 5, label: "先月比" }}
      />
      <StatsCard
        title="下書き"
        value={props.stats.drafts}
      />
      <StatsCard
        title="今月の投稿"
        value={props.stats.monthlyPosts}
        trend={{ value: -8, label: "先月比" }}
      />
    </div>

    <div className={styles["content-grid"]}>
      <div className={styles.section}>
        <div className={styles["section-header"]}>
          <h2 className={styles["section-title"]}>最近のコンテンツ</h2>
          <a href="/admin/articles" className={styles["section-link"]}>
            すべて表示
          </a>
        </div>
        <div className={styles["section-content"]}>
          <RecentList items={props.recentItems} />
        </div>
      </div>

      <div className={styles.section}>
        <div className={styles["section-header"]}>
          <h2 className={styles["section-title"]}>アクティビティ</h2>
        </div>
        <div className={styles["section-content"]}>
          <ActivityList activities={props.activities} />
        </div>
      </div>
    </div>
  </div>
);
