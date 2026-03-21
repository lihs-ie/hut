"use client";

import styles from "./edit.module.css";
import { useCallback, useState } from "react";
import { ulid } from "ulid";
import { PublishStatus } from "@shared/domains/common";
import { useServerAction } from "@shared/components/global/hooks/use-server-action";
import { ErrorModal } from "@shared/components/molecules/modal/error";
import { Series, UnvalidatedSeries } from "@shared/domains/series";
import { TagSelect } from "@shared/components/molecules/select/tag";
import { Tag, TagIdentifier } from "@shared/domains/attributes/tag";
import { TagIcon } from "@shared/components/atoms/icon/tag";
import { CrossIcon } from "@shared/components/atoms/icon/cross";
import { LoadingOverlay } from "@shared/components/molecules/overlay/loading";
import { useToast } from "@shared/components/molecules/toast";

export type Props = {
  initial?: Series;
  persist: (unvalidated: UnvalidatedSeries) => Promise<void>;
  tags: Tag[];
};

export const SeriesEdit = (props: Props) => {
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
      onSuccess: () =>
        showToast(props.initial ? "連載を更新しました" : "連載を作成しました"),
    },
  );

  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <h1 className={styles.heading}>
          {props.initial ? "連載を編集" : "連載を作成"}
        </h1>
      </div>

      <div className={styles.form}>
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
            カバー画像URL
          </label>
          <input
            id="cover"
            name="cover"
            type="url"
            value={cover}
            onChange={(event) => setCover(event.target.value)}
            placeholder="https://example.com/cover.png"
            className={styles.input}
          />
        </div>

        <div className={styles.field}>
          <div className={styles["tags-header"]}>
            <span className={styles["tags-icon"]}>
              <TagIcon />
            </span>
            <label className={styles.label}>タグ</label>
          </div>
          {tags.length > 0 && (
            <div className={styles["tags-selected-wrapper"]}>
              <div className={styles["tags-selected"]}>
                {tags.map((tagIdentifier) => {
                  const tag = props.tags.find(
                    (t) => t.identifier === tagIdentifier,
                  );
                  if (!tag) return null;
                  return (
                    <span key={tagIdentifier} className={styles["tag-item"]}>
                      {tag.name}
                      <button
                        type="button"
                        className={styles["tag-remove"]}
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
                className={styles["tags-clear"]}
                onClick={() => handleTagsChange([])}
              >
                全て解除
              </button>
            </div>
          )}
          {props.tags.filter((t) => !tags.includes(t.identifier)).length >
            0 && (
            <>
              <p className={styles["tags-available-label"]}>
                クリックして追加
              </p>
              <TagSelect
                tags={props.tags.filter((t) => !tags.includes(t.identifier))}
                onSelect={(tag) =>
                  handleTagsChange([...tags, tag.identifier])
                }
                selected={tags}
              />
            </>
          )}
        </div>

        <div className={styles.field}>
          <div className={styles["status-row"]}>
            <label htmlFor="status" className={styles.label}>
              公開ステータス
            </label>
            <label className={styles["status-toggle"]}>
              <input
                id="status"
                type="checkbox"
                checked={status === PublishStatus.PUBLISHED}
                onChange={(event) =>
                  setStatus(
                    event.target.checked
                      ? PublishStatus.PUBLISHED
                      : PublishStatus.DRAFT,
                  )
                }
                className={styles["status-checkbox"]}
              />
              <span className={styles["status-label"]}>
                {status === PublishStatus.PUBLISHED ? "公開" : "下書き"}
              </span>
            </label>
          </div>
        </div>

        <div className={styles.actions}>
          <button
            type="button"
            onClick={execute}
            disabled={isLoading || !title.trim()}
            className={styles["save-button"]}
          >
            {isLoading ? "保存中..." : "保存"}
          </button>
        </div>
      </div>

      {isLoading && <LoadingOverlay />}
      {error && (
        <ErrorModal message={error.message} onClose={reset} isOpen={!!error} />
      )}
    </div>
  );
};
