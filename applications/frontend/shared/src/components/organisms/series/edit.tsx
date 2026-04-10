"use client";

import Link from "next/link";
import Image from "next/image";
import { useRouter } from "next/navigation";
import styles from "./edit.module.css";
import { useCallback, useMemo, useState } from "react";
import { ulid } from "ulid";
import { PublishStatus } from "@shared/domains/common";
import { useServerAction } from "@shared/components/global/hooks/use-server-action";
import { useImageUpload } from "@shared/components/global/hooks/use-image-upload";
import { ErrorModal } from "@shared/components/molecules/modal/error";
import { Series, SeriesSlug, UnvalidatedSeries } from "@shared/domains/series";
import { Chapter } from "@shared/domains/series/chapter";
import { TagSelect } from "@shared/components/molecules/select/tag";
import { Tag, TagIdentifier } from "@shared/domains/attributes/tag";
import { TagIcon } from "@shared/components/atoms/icon/tag";
import { CrossIcon } from "@shared/components/atoms/icon/cross";
import { PlusIcon } from "@shared/components/atoms/icon/plus";
import { BallpenIcon } from "@shared/components/atoms/icon/ballpen";
import { TrashIcon } from "@shared/components/atoms/icon/trash";
import { LoadingOverlay } from "@shared/components/molecules/overlay/loading";
import { useToast } from "@shared/components/molecules/toast";
import { Routes } from "@shared/config/presentation/route";

export type Props = {
  initial?: Series;
  persist: (unvalidated: UnvalidatedSeries) => Promise<void>;
  tags: Tag[];
  chapters?: Chapter[];
  seriesSlug?: SeriesSlug;
  terminateChapter?: (chapterIdentifier: string, seriesSlug: string) => Promise<void>;
  uploadImage?: (file: File | Blob, path: string) => Promise<string>;
};

export const SeriesEditOrganism = (props: Props) => {
  const router = useRouter();
  const { showToast } = useToast();

  const [identifier] = useState(() => props.initial?.identifier ?? ulid());
  const [title, setTitle] = useState<string>(props.initial?.title ?? "");
  const [slug, setSlug] = useState<string>(props.initial?.slug ?? "");
  const [subTitle, setSubTitle] = useState<string>(
    props.initial?.subTitle ?? "",
  );
  const [description, setDescription] = useState<string>(
    props.initial?.description ?? "",
  );
  const [cover, setCover] = useState<string>(props.initial?.cover ?? "");
  const [status, setStatus] = useState<PublishStatus>(
    props.initial?.status ?? PublishStatus.DRAFT,
  );
  const [tags, setTags] = useState<TagIdentifier[]>(
    props.initial?.tags ?? [],
  );

  const handleTagsChange = useCallback((newTags: TagIdentifier[]) => {
    setTags(newTags);
  }, []);

  const noopUploadAction = useMemo(() => async () => "", []);

  const imageUploadHook = useImageUpload({
    uploadAction: props.uploadImage ?? noopUploadAction,
  });

  const handleCoverFileChange = useCallback(
    async (event: React.ChangeEvent<HTMLInputElement>) => {
      if (!props.uploadImage) return;
      const file = event.target.files?.[0];
      if (!file) return;
      const result = await imageUploadHook.uploadImage(file, "series", identifier);
      if (result.isOk) {
        setCover(result.unwrap().url);
      }
    },
    [props.uploadImage, imageUploadHook, identifier],
  );

  const { execute, error, isLoading, reset } = useServerAction(
    async () => {
      await props.persist({
        identifier,
        title,
        slug: props.initial?.slug ?? slug,
        subTitle: subTitle || null,
        description: description || undefined,
        cover: cover || null,
        tags,
        chapters: props.initial?.chapters ?? [],
        status,
        timeline: {
          createdAt: props.initial?.timeline.createdAt ?? new Date(),
          updatedAt: new Date(),
        },
      });
    },
    {
      onSuccess: () => {
        showToast(props.initial ? "連載を更新しました" : "連載を作成しました");
        if (!props.initial) {
          router.push(Routes.page.series.edit(slug));
        }
      },
    },
  );

  const isPublished = status === PublishStatus.PUBLISHED;

  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <h1 className={styles.heading}>
          {props.initial ? "連載を編集" : "連載を作成"}
        </h1>
      </div>

      <div className={styles.layout}>
        <div className={styles.main}>
          <div className={styles.card}>
            <div className={styles.field}>
            <label htmlFor="title" className={styles.label}>
              タイトル
            </label>
            <input
              id="title"
              name="title"
              type="text"
              value={title}
              onChange={(event) => setTitle(event.target.value)}
              placeholder="連載タイトルを入力"
              className={styles.input}
            />
          </div>

          <div className={styles.field}>
            <label htmlFor="slug" className={styles.label}>
              スラッグ
            </label>
            <input
              id="slug"
              name="slug"
              type="text"
              value={slug}
              onChange={(event) => setSlug(event.target.value)}
              placeholder="series-slug"
              className={styles.input}
              disabled={!!props.initial}
            />
          </div>

          <div className={styles.field}>
            <label htmlFor="subTitle" className={styles.label}>
              サブタイトル
            </label>
            <input
              id="subTitle"
              name="subTitle"
              type="text"
              value={subTitle}
              onChange={(event) => setSubTitle(event.target.value)}
              placeholder="サブタイトルを入力（任意）"
              className={styles.input}
            />
          </div>

          <div className={styles.field}>
            <label htmlFor="description" className={styles.label}>
              説明
            </label>
            <textarea
              id="description"
              name="description"
              value={description}
              onChange={(event) => setDescription(event.target.value)}
              placeholder="連載の説明を入力（任意）"
              className={styles.textarea}
              rows={4}
            />
          </div>

          <div className={styles.field}>
            <label htmlFor="cover" className={styles.label}>
              カバー画像
            </label>
            {props.uploadImage && (
              <input
                id="cover"
                type="file"
                accept="image/jpeg,image/png,image/webp,image/gif"
                onChange={handleCoverFileChange}
                className={styles.input}
              />
            )}
            {!props.uploadImage && (
              <input
                id="cover"
                name="cover"
                type="url"
                value={cover}
                onChange={(event) => setCover(event.target.value)}
                placeholder="https://example.com/cover.png"
                className={styles.input}
              />
            )}
            {cover && (
              <Image
                src={cover}
                alt="カバー画像プレビュー"
                width={320}
                height={180}
                unoptimized
                className={styles.coverpreview}
              />
            )}
          </div>

          <div className={styles.field}>
            <div className={styles.tagsheader}>
              <span className={styles.tagsicon}>
                <TagIcon />
              </span>
              <label className={styles.label}>タグ</label>
            </div>
            {tags.length > 0 && (
              <div className={styles.tagswrapper}>
                <div className={styles.tagsselected}>
                  {tags.map((tagIdentifier) => {
                    const tag = props.tags.find(
                      (t) => t.identifier === tagIdentifier,
                    );
                    if (!tag) return null;
                    return (
                      <span key={tagIdentifier} className={styles.tagitem}>
                        {tag.name}
                        <button
                          type="button"
                          className={styles.tagremove}
                          onClick={() =>
                            handleTagsChange(
                              tags.filter((t) => t !== tagIdentifier),
                            )
                          }
                          aria-label={`${tag.name}を削除`}
                        >
                          <CrossIcon />
                        </button>
                      </span>
                    );
                  })}
                </div>
                <button
                  type="button"
                  className={styles.tagsclear}
                  onClick={() => handleTagsChange([])}
                >
                  全て解除
                </button>
              </div>
            )}
            {props.tags.filter((t) => !tags.includes(t.identifier)).length >
              0 && (
              <>
                <p className={styles.tagsavailablelabel}>
                  クリックして追加
                </p>
                <div className={styles.tagsavailable}>
                  <TagSelect
                    tags={props.tags.filter((t) => !tags.includes(t.identifier))}
                    onSelect={(tag) =>
                      handleTagsChange([...tags, tag.identifier])
                    }
                    selected={tags}
                  />
                </div>
              </>
            )}
          </div>

          <div className={`${styles.field} ${styles.last}`}>
            <div className={styles.statusrow}>
              <label htmlFor="status" className={styles.label}>
                公開ステータス
              </label>
              <div className={styles.statuscontrol}>
                <button
                  id="status"
                  type="button"
                  role="switch"
                  aria-checked={isPublished}
                  className={`${styles.toggle} ${isPublished ? styles.checked : ""}`}
                  onClick={() =>
                    setStatus(
                      isPublished ? PublishStatus.DRAFT : PublishStatus.PUBLISHED,
                    )
                  }
                >
                  <span className={styles.togglethumb} />
                </button>
                <span
                  className={`${styles.statusbadge} ${isPublished ? styles.published : styles.draft}`}
                >
                  {isPublished ? "公開中" : "下書き"}
                </span>
              </div>
            </div>
          </div>
        </div>

          <div className={styles.actions}>
            <button
              type="button"
              onClick={execute}
              disabled={isLoading || !title.trim()}
              className={styles.savebutton}
            >
              {isLoading ? "保存中..." : "保存"}
            </button>
          </div>
        </div>

        {props.chapters !== undefined && props.seriesSlug !== undefined && (
          <aside className={styles.sidebar}>
            <div className={styles.chapterssection}>
            <div className={styles.chapterssectionheader}>
            <h2 className={styles.chapterssectiontitle}>チャプター管理</h2>
            <Link
              href={Routes.page.series.chapter.new(props.seriesSlug)}
              className={styles.addchapterlink}
            >
              <PlusIcon className={styles.addicon} />
              チャプターを追加
            </Link>
          </div>
          {props.chapters.length === 0 ? (
            <div className={styles.chaptersempty}>
              まだチャプターがありません
            </div>
          ) : (
            <ol className={styles.chapterslist}>
              {props.chapters.map((chapter, index) => (
                <li key={chapter.slug} className={styles.chapteritem}>
                  <span className={styles.chapternumber}>
                    {String(index + 1).padStart(2, "0")}
                  </span>
                  <span className={styles.chaptertitle}>{chapter.title}</span>
                  <Link
                    href={Routes.page.series.chapter.edit(props.seriesSlug!, chapter.slug)}
                    className={styles.chapterlink}
                  >
                    <BallpenIcon className={styles.chaptericon} />
                    編集
                  </Link>
                  {props.terminateChapter && (
                    <button
                      type="button"
                      className={styles.chapterdelete}
                      onClick={() => {
                        if (!window.confirm(`「${chapter.title}」を削除しますか？`)) return;
                        props.terminateChapter!(chapter.identifier, props.seriesSlug!);
                      }}
                      aria-label={`${chapter.title}を削除`}
                    >
                      <TrashIcon className={styles.chaptericon} />
                    </button>
                  )}
                </li>
              ))}
            </ol>
          )}
            </div>
          </aside>
        )}
      </div>

      {isLoading && <LoadingOverlay />}
      {error && (
        <ErrorModal message={error.message} onClose={reset} isOpen={!!error} />
      )}
    </div>
  );
};
