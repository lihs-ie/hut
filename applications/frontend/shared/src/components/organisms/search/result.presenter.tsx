import { Article, articleSchema } from "@shared/domains/articles";
import { Memo, memoSchema } from "@shared/domains/memo";
import { Series } from "@shared/domains/series";
import styles from "./result.module.css";
import { HomeContentCard } from "@shared/components/molecules/list/card/home-content";
import { ContentType, UnvalidatedCriteria } from "@shared/domains/search-token";
import { TagName } from "@shared/domains/attributes/tag";
import { SearchEmpty } from "@shared/components/molecules/empty/search";

type ContentWithTagNames = (Article | Series | Memo) & { tagNames: TagName[] };

export type Props = {
  contents: ContentWithTagNames[];
  criteria: UnvalidatedCriteria;
};

const hasSearchCriteria = (criteria: UnvalidatedCriteria): boolean => {
  return (
    criteria.freeWord !== null ||
    (criteria.tags !== null && criteria.tags.length > 0) ||
    criteria.type !== null
  );
};

const determineType = (
  content: Article | Series | Memo,
): ContentType => {
  if (articleSchema.safeParse(content).success) {
    return ContentType.ARTICLE;
  } else if (memoSchema.safeParse(content).success) {
    return ContentType.MEMO;
  } else {
    return ContentType.SERIES;
  }
};

export const SearchResultPresenter = (props: Props) => {
  const variant = hasSearchCriteria(props.criteria) ? "empty" : "initial";

  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <span>検索結果：{props.contents.length}件</span>
      </div>
      {props.contents.length === 0 ? (
        <SearchEmpty variant={variant} />
      ) : (
        <div className={styles.contents}>
          {props.contents.map((content) => (
            <HomeContentCard
              key={content.slug}
              slug={content.slug}
              type={determineType(content)}
              title={content.title}
              date={content.timeline.createdAt}
              tagNames={content.tagNames}
            />
          ))}
        </div>
      )}
    </div>
  );
};
