import { Article } from "@shared/domains/articles";
import { Memo } from "@shared/domains/memo";
import { Series } from "@shared/domains/series";
import styles from "./index.module.css";
import { Profile } from "@shared/domains/user";
import { ContentSection } from "@shared/components/organisms/common/top/search";
import { ContentSectionSkeleton } from "@shared/components/organisms/common/top/search.skeleton";
import { ProfileCard } from "@shared/components/molecules/list/card/profile";
import { ContentType } from "@shared/domains/search-token";
import { Tag } from "@shared/domains/attributes/tag";
import { Suspense } from "react";
import { isPublished } from "@shared/domains/common";

export type Props = {
  searchArticles: () => Promise<Article[]>;
  searchMemos: () => Promise<Memo[]>;
  searchSeries: () => Promise<Series[]>;
  getProfile: () => Promise<Profile>;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
};

export const TopIndex = async (props: Props) => {
  const profile = await props.getProfile();

  return (
    <div className={styles.container}>
      <Suspense fallback={<ContentSectionSkeleton count={6} />}>
        <ContentSection
          search={async () => (await props.searchArticles()).filter(isPublished)}
          type={ContentType.ARTICLE}
          titleOf={(article) => article.title}
          dateOf={(article) => article.publishedAt}
          slugOf={(article) => article.slug}
          findAllTags={props.findAllTags}
          hasAllLink
        />
      </Suspense>
      <Suspense fallback={<ContentSectionSkeleton count={6} />}>
        <ContentSection
          search={async () => (await props.searchMemos()).filter(isPublished)}
          type={ContentType.MEMO}
          titleOf={(memo) => memo.title}
          dateOf={(memo) => memo.publishedAt}
          slugOf={(memo) => memo.slug}
          findAllTags={props.findAllTags}
          hasAllLink
        />
      </Suspense>
      <Suspense fallback={<ContentSectionSkeleton count={6} />}>
        <ContentSection
          search={async () => (await props.searchSeries()).filter(isPublished)}
          type={ContentType.SERIES}
          titleOf={(series) => series.title}
          dateOf={(series) => series.publishedAt}
          slugOf={(series) => series.slug}
          findAllTags={props.findAllTags}
          hasAllLink
        />
      </Suspense>
      <ProfileCard profile={profile} />
    </div>
  );
};
