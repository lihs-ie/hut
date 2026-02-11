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
            <ClockIcon className={styles.clockIcon} />
            <span>
              投稿日時：{formatDate(props.timeline.createdAt)}
            </span>
          </ModestText>
          <ModestText>
            <ClockIcon className={styles.clockIcon} />
            <span>
              最終更新日時：{formatDate(props.timeline.updatedAt)}
            </span>
          </ModestText>
        </div>
      </div>
    </div>
  </div>
);
