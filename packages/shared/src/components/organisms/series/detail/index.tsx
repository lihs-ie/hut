import Link from "next/link";
import Image from "next/image";
import { Series, SeriesSlug } from "@shared/domains/series";
import { SimpleBadge } from "@shared/components/atoms/badge/simple";
import styles from "./index.module.css";
import { BookOpenIcon } from "@shared/components/atoms/icon/facing-book";
import { ChevronRightIcon } from "@shared/components/atoms/icon/chevron-right";

export type Props = {
  series: Series;
  slug: SeriesSlug;
  author?: {
    name: string;
    avatar?: string;
    bio?: string;
  };
};

export const SeriesDetail = (props: Props) => {
  const series = props.series;
  const firstChapter = series.chapters[0];

  const formatDate = (date: Date) => {
    return `${date.getFullYear()}/${String(date.getMonth() + 1).padStart(2, "0")}/${String(date.getDate()).padStart(2, "0")}`;
  };

  return (
    <div className={styles.container}>
      <aside className={styles.sidebar}>
        <div className={styles["sidebar-card"]}>
          <div className={styles["sidebar-inner"]}>
            <div className={styles["cover-wrapper"]}>
              {series.cover ? (
                <Image
                  src={series.cover}
                  alt={series.title}
                  fill
                  className={styles["cover-image"]}
                />
              ) : (
                "📚"
              )}
            </div>

            <div className={styles["book-info"]}>
              <h2 className={styles["book-title"]}>{series.title}</h2>
              {series.subTitle && (
                <p className={styles["book-subtitle"]}>{series.subTitle}</p>
              )}

              {props.author && (
                <div className={styles["author-info"]}>
                  {props.author.avatar && (
                    <div className={styles["author-avatar"]}>
                      <Image
                        src={props.author.avatar}
                        alt={props.author.name}
                        width={32}
                        height={32}
                      />
                    </div>
                  )}
                  <p className={styles["author-name"]}>{props.author.name}</p>
                </div>
              )}

              <div className={styles.metadata}>
                <p>公開: {formatDate(series.timeline.createdAt)}</p>
              </div>

              {firstChapter && (
                <Link
                  href={`/series/${props.slug}/chapters/${firstChapter.slug}`}
                  className={styles["read-button"]}
                >
                  <BookOpenIcon className={styles["button-icon"]} />
                  今すぐ読む
                </Link>
              )}
            </div>
          </div>
        </div>
      </aside>

      {/* メインコンテンツ */}
      <main className={styles.main}>
        <div className={styles.header}>
          <h1 className={styles["main-title"]}>{series.title}</h1>
          {series.subTitle && (
            <p className={styles["main-subtitle"]}>{series.subTitle}</p>
          )}

          <div className={styles["topic-list"]}>
            {series.tags.map((tag) => (
              <SimpleBadge key={tag} label={tag} />
            ))}
          </div>

          {series.description && (
            <p className={styles.description}>{series.description}</p>
          )}
        </div>

        {/* チャプター一覧 */}
        <div className={styles["chapters-section"]}>
          <h2 className={styles["chapters-title"]}>目次</h2>
          <div className={styles["chapter-list"]}>
            {series.chapters.map((chapter, index) => (
              <Link
                key={chapter.slug}
                href={`/series/${props.slug}/chapters/${chapter.slug}`}
                className={styles["chapter-card"]}
              >
                <div className={styles["chapter-content"]}>
                  <div className={styles["chapter-info"]}>
                    <div className={styles["chapter-number"]}>{index + 1}</div>
                    <h3 className={styles["chapter-title"]}>{chapter.title}</h3>
                  </div>
                  <ChevronRightIcon className={styles["chapter-icon"]} />
                </div>
              </Link>
            ))}
          </div>
        </div>

        {/* 著者情報 */}
        {props.author && (
          <div className={styles["author-card"]}>
            <h3 className={styles["author-card-title"]}>著者について</h3>
            <div className={styles["author-card-content"]}>
              {props.author.avatar && (
                <div className={styles["author-card-avatar"]}>
                  <Image
                    src={props.author.avatar}
                    alt={props.author.name}
                    width={64}
                    height={64}
                  />
                </div>
              )}
              <div className={styles["author-card-info"]}>
                <p className={styles["author-card-name"]}>
                  {props.author.name}
                </p>
                {props.author.bio && (
                  <p className={styles["author-card-bio"]}>
                    {props.author.bio}
                  </p>
                )}
              </div>
            </div>
          </div>
        )}
      </main>
    </div>
  );
};
