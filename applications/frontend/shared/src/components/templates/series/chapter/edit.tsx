import { ChapterEditOrganism } from "@shared/components/organisms/series/chapter/edit";
import { Chapter, UnvalidatedChapter } from "@shared/domains/series/chapter";
import styles from "./edit.module.css";

export type Props = {
  initial?: Chapter;
  persist: (unvalidated: UnvalidatedChapter) => Promise<void>;
  uploadImage: (file: File | Blob, path: string) => Promise<string>;
  seriesSlug: string;
};

export const ChapterEdit = (props: Props) => (
  <div className={styles.container}>
    <ChapterEditOrganism
      initial={props.initial}
      persist={props.persist}
      uploadImage={props.uploadImage}
      seriesSlug={props.seriesSlug}
    />
  </div>
);
