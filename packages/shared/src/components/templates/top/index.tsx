import { Article } from "@shared/domains/articles";
import { Memo } from "@shared/domains/memo";
// [初期リリース対象外] import { Series } from "@shared/domains/series";
import styles from "./index.module.css";
import { Profile } from "@shared/domains/user";
import { ContentSection } from "@shared/components/organisms/common/top/search";
import { ContentSectionSkeleton } from "@shared/components/organisms/common/top/search.skeleton";
import { ProfileCard } from "@shared/components/molecules/list/card/profile";
import { ContentType } from "@shared/domains/search-token";
import { Tag } from "@shared/domains/attributes/tag";
import { Suspense } from "react";

export type Props = {
  searchArticles: () => Promise<Article[]>;
  searchMemos: () => Promise<Memo[]>;
  // [初期リリース対象外] searchSeries: () => Promise<Series[]>;
  getProfile: () => Promise<Profile>;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
};

export const TopIndex = async (props: Props) => {
  const profile = await props.getProfile();

  return (
    <div className={styles.container}>
      <Suspense fallback={<ContentSectionSkeleton count={6} />}>
        <ContentSection
          search={props.searchArticles}
          type={ContentType.ARTICLE}
          titleOf={(article) => article.title}
          dateOf={(article) => article.timeline.createdAt}
          slugOf={(article) => article.slug}
          findAllTags={props.findAllTags}
          hasAllLink
        />
      </Suspense>
      <Suspense fallback={<ContentSectionSkeleton count={6} />}>
        <ContentSection
          search={props.searchMemos}
          type={ContentType.MEMO}
          titleOf={(memo) => memo.title}
          dateOf={(memo) => memo.timeline.createdAt}
          slugOf={(memo) => memo.slug}
          findAllTags={props.findAllTags}
          hasAllLink
        />
      </Suspense>
      {/* [初期リリース対象外] シリーズセクション
      <Suspense fallback={<ContentSectionSkeleton count={6} />}>
        <ContentSection
          search={props.searchSeries}
          type={ContentType.SERIES}
          titleOf={(series) => series.title}
          dateOf={(series) => series.timeline.createdAt}
          slugOf={(series) => series.slug}
          findAllTags={props.findAllTags}
          hasAllLink
        />
      </Suspense>
      */}
      <ProfileCard profile={profile} />
    </div>
  );
};
