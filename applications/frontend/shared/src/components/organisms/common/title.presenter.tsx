import { Timeline } from "@shared/domains/common";

import styles from "./title.module.css";
import { ClockIcon } from "@shared/components/atoms/icon/clock";
import { ModestText } from "@shared/components/atoms/text/modest";
import { formatDate } from "@shared/aspects/date";

export type Props = {
  title: string;
  timeline: Timeline;
};

export const TitlePresenter = (props: Props) => (
  <div className={styles.container}>
    <div className={styles.content}>
      <h1 className={styles.title}>{props.title}</h1>
      <div className={styles.meta}>
        <div className={styles.publishedDate}>
          <ModestText>
            <div className={styles.icon}>
              <ClockIcon />
            </div>
            投稿日時：{formatDate(props.timeline.createdAt)}
          </ModestText>
          <ModestText>
            <div className={styles.icon}>
              <ClockIcon />
            </div>
              最終更新日時：{formatDate(props.timeline.updatedAt)}
          </ModestText>
        </div>
      </div>
    </div>
  </div>
);
