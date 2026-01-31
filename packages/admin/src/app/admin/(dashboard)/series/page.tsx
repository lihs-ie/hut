// [初期リリース対象外] シリーズ管理ページ
// import { SeriesListIndex } from "@shared/components/templates/series/list";
// import { search } from "@shared/actions/series";
// import { findAllTags } from "@shared/actions/tag";
// import { Suspense } from "react";
// import { ContentCardSkeletonList } from "@shared/components/molecules/skeleton";
import { notFound } from "next/navigation";

export default function AdminSeriesPage() {
  // [初期リリース対象外]
  // return (
  //   <Suspense fallback={<ContentCardSkeletonList count={6} />}>
  //     <SeriesListIndex
  //       search={() => search({ slug: null, tags: null })}
  //       findAllTags={findAllTags}
  //     />
  //   </Suspense>
  // );
  return notFound();
}
