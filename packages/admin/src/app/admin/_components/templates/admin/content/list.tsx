import { Tag } from "@shared/domains/attributes/tag";
import { ContentType } from "@shared/domains/search-token";
import { AdminSearch } from "../../../organisms/search";
import { Props as ContentsProps } from "@/app/admin/_components/molecules/list/content";
import styles from "./list.module.css";

export type Props<Content, Criteria> = {
  title: string;
  getAllTags: () => Promise<Tag[]>;
  searchContents: (criteria: Criteria) => Promise<Content[]>;
  unvalidated: Criteria;
  contentType: ContentType;
  valuesOf: (content: Content) => ContentsProps["contents"][number];
};

export const AdminContentListIndex = async <Content, Criteria>(
  props: Props<Content, Criteria>,
) => {
  return (
    <div className={styles.container}>
      <AdminSearch {...props} />
    </div>
  );
};
