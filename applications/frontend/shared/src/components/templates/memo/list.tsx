import {
  ContentSection,
  type Props as ContentSectionProps,
} from "@shared/components/organisms/common/top/search";
import { Tag } from "@shared/domains/attributes/tag";
import { isPublished } from "@shared/domains/common";
import { ContentType } from "@shared/domains/search-token";
import styles from "./list.module.css";
import { Memo } from "@shared/domains/memo";

export type Props = {
  search: () => Promise<Memo[]>;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
  linkMode?: ContentSectionProps<Memo>["linkMode"];
};

export const MemoListIndex = (props: Props) => (
  <div className={styles.container}>
    <ContentSection
      search={async () => (await props.search()).filter(isPublished)}
      findAllTags={props.findAllTags}
      slugOf={(memo) => memo.slug}
      titleOf={(memo) => memo.title}
      dateOf={(memo) => memo.publishedAt}
      type={ContentType.MEMO}
      maxItems={Infinity}
      linkMode={props.linkMode}
    />
  </div>
);
