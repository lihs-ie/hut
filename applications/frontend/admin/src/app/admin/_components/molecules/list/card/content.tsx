import { PublishStatus } from "@shared/domains/common";
import styles from "./content.module.css";
import { PlayButton } from "../../button/play";
import { EditButton } from "../../button/edit";
import { EraserButton } from "../../button/eraser";
import { PublishStatusBadge } from "../../../atoms/badge/status";
import { NavigableLink } from "@shared/components/molecules/link/navigable";

export type Props = {
  title: string;
  status: PublishStatus;
  previewHref: string;
  editHref: string;
  updatedAt: Date;
  onTerminate: () => void;
};

export const AdminContentCard = (props: Props) => (
  <div className={styles.container}>
    <article className={styles.texts}>
      <NavigableLink href={props.previewHref} className={styles.title}>
        {props.title}
      </NavigableLink>
      <footer className={styles.statuses}>
        <PublishStatusBadge status={props.status} />
        <span className={styles.updatedAt}>
          最終更新: {props.updatedAt.toLocaleDateString()}
        </span>
      </footer>
    </article>
    <div className={styles.actions}>
      <PlayButton href={props.previewHref} />
      <EditButton href={props.editHref} />
      <EraserButton onClick={props.onTerminate} />
    </div>
  </div>
);
