import { MarkdownRenderer } from "@shared/components/global/mdx";
import { Memo, MemoEntry, UnvalidatedEntry } from "@shared/domains/memo";
import { ImageIdentifier } from "@shared/domains/image";
import styles from "./edit.module.css";
import { MemoEntries } from "@shared/components/organisms/memo/edit/list";
import { EntryEditor } from "@shared/components/organisms/memo/edit/entry";
import {
  MemoEditSidebar,
  Props as SidebarProps,
} from "@shared/components/organisms/memo/edit/sidebar";
import { Suspense } from "react";
import {
  ArticleTitleSkeleton,
  ArticleContentSkeleton,
} from "@shared/components/molecules/skeleton";
import { Title } from "@shared/components/organisms/common/title";
import { Slug } from "@shared/domains/common";

export type Props = {
  slug: Slug;
  uploadAction: (file: File | Blob, path: string) => Promise<string>;
  renderer: MarkdownRenderer;
  getEntries: (slug: string) => Promise<MemoEntry[]>;
  persistEntry: (unvalidated: UnvalidatedEntry, slug: string, images: ImageIdentifier[]) => Promise<void>;
  findBySlug: (slug: string) => Promise<Memo>;
  sidebar: SidebarProps;
};

export const MemoEditIndex = async (props: Props) => {
  const memo = await props.findBySlug(props.slug);
  return (
    <main className={styles.container}>
      <Suspense fallback={<ArticleTitleSkeleton />}>
        <Title
          find={props.findBySlug}
          slug={props.slug}
          titleOf={(memo) => memo.title}
          timelineOf={(memo) => memo.timeline}
        />
      </Suspense>
      <div className={styles.content}>
        <div className={styles.entries}>
          <Suspense fallback={<ArticleContentSkeleton />}>
            <MemoEntries
              getEntries={props.getEntries}
              slug={props.slug}
              renderer={props.renderer}
            />
          </Suspense>

          <EntryEditor
            persist={props.persistEntry}
            slug={props.slug}
            memoIdentifier={memo.identifier}
            uploadAction={props.uploadAction}
          />
        </div>

        <div className={styles.sidebar}>
          <MemoEditSidebar {...props.sidebar} />
        </div>
      </div>
    </main>
  );
};
