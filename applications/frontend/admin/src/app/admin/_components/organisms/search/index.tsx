import { Tag } from "@shared/domains/attributes/tag";
import { AdminSearchHeader } from "./header";
import { AdminSearchHeaderSkeleton } from "./header.skeleton";
import { ContentType } from "@shared/domains/search-token";
import {
  AdminContentList,
  Props as ContentsProps,
} from "@/app/admin/_components/molecules/list/content";
import { AdminContentListSkeleton } from "@/app/admin/_components/molecules/list/content.skeleton";
import styles from "./index.module.css";
import { Suspense } from "react";

export type Props<Content, Criteria> = {
  title: string;
  getAllTags: () => Promise<Tag[]>;
  searchContents: (criteria: Criteria) => Promise<Content[]>;
  unvalidated: Criteria;
  contentType: ContentType;
  valuesOf: (content: Content) => ContentsProps["contents"][number];
};

export const AdminSearch = async <Content, Criteria>(
  props: Props<Content, Criteria>,
) => {
  const contents = await props.searchContents(props.unvalidated);

  return (
    <div className={styles.container}>
      <Suspense fallback={<AdminSearchHeaderSkeleton />}>
        <AdminSearchHeader
          getAllTags={props.getAllTags}
          title={props.title}
          contentType={props.contentType}
          unvalidated={props.unvalidated}
        />
      </Suspense>
      <Suspense fallback={<AdminContentListSkeleton />}>
        <AdminContentList
          key={JSON.stringify(props.unvalidated)}
          contents={contents.map((content) => props.valuesOf(content))}
        />
      </Suspense>
    </div>
  );
};
