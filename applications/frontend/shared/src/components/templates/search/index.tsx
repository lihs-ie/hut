import styles from "./index.module.css";
import { UnvalidatedCriteria } from "@shared/domains/search-token";
import { Suspense } from "react";
import { SearchResult } from "@shared/components/organisms/search/result";
import { SearchResultSkeleton } from "@shared/components/organisms/search/result.skeleton";
import { Article } from "@shared/domains/articles";
import { Series } from "@shared/domains/series";
import { Memo } from "@shared/domains/memo";
import { Tag } from "@shared/domains/attributes/tag";
import { SearchFilter } from "@shared/components/organisms/search/filters";
import { SearchFilterSkeleton } from "@shared/components/organisms/search/filters.skeleton";
import { SearchErrorBoundary } from "@shared/components/molecules/boundary/search-error";

export type Props = {
  unvalidatedCriteria: UnvalidatedCriteria;
  search: (
    unvalidated: UnvalidatedCriteria,
  ) => Promise<(Article | Series | Memo)[]>;
  getAllTags: () => Promise<Tag[]>;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
  ofNamesTags: (names: string[]) => Promise<Tag[]>;
};

export const SearchIndex = (props: Props) => {
  const searchKey = JSON.stringify(props.unvalidatedCriteria);

  return (
    <div className={styles.container}>
      <div className={styles.main}>
        <Suspense fallback={<SearchFilterSkeleton />}>
          <SearchFilter getAllTags={props.getAllTags} />
        </Suspense>
        <SearchErrorBoundary>
          <Suspense key={searchKey} fallback={<SearchResultSkeleton />}>
            <SearchResult
              search={props.search}
              unvalidatedCriteria={props.unvalidatedCriteria}
              findAllTags={props.findAllTags}
              ofNamesTags={props.ofNamesTags}
            />
          </Suspense>
        </SearchErrorBoundary>
      </div>
    </div>
  );
};
