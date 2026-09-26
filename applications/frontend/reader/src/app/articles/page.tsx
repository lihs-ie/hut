import { browseArticlesPage } from "@/actions/feed/article-search";
import { findAllTags } from "@/actions/tag";
import { ArticleListIndex } from "@shared/components/templates/article/list";
import { ChevronLeftIcon } from "@shared/components/atoms/icon/chevron-left";
import { ChevronRightIcon } from "@shared/components/atoms/icon/chevron-right";
import Link from "next/link";
import { redirect } from "next/navigation";
import styles from "./page.module.css";

export const revalidate = 60;

const pageSize = 20;

type Props = {
  searchParams: Promise<{ page?: string }>;
};

/** Lists one published Article page with bounded navigation. */
export default async function Page({ searchParams }: Props) {
  const rawPage = (await searchParams).page ?? "1";
  const requestedPage = /^[1-9][0-9]*$/.test(rawPage)
    ? Number(rawPage)
    : 1;
  const page = Number.isSafeInteger(requestedPage) ? requestedPage : 1;
  const result = await browseArticlesPage(page, pageSize);
  const lastPage = Math.max(1, Math.ceil(result.total / pageSize));
  if (page > lastPage) redirect(`/articles?page=${lastPage}`);

  return (
    <>
      <ArticleListIndex search={async () => result.articles} findAllTags={findAllTags} />
      {lastPage > 1 && (
        <nav className={styles.pagination} aria-label="記事のページ切り替え">
          {page > 1 ? (
            <Link href={`/articles?page=${page - 1}`} className={styles.link}>
              <ChevronLeftIcon />前へ
            </Link>
          ) : <span className={styles.spacer} />}
          <span className={styles.position}>{page} / {lastPage}</span>
          {page < lastPage ? (
            <Link href={`/articles?page=${page + 1}`} className={styles.link}>
              次へ<ChevronRightIcon />
            </Link>
          ) : <span className={styles.spacer} />}
        </nav>
      )}
    </>
  );
}
