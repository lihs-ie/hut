"use client";

import styles from "./edit.module.css";
import dynamic from "next/dynamic";
import { useRouter } from "next/navigation";
import { useCallback, useState } from "react";
import { ulid } from "ulid";
import { PublishStatus } from "@shared/domains/common";
import { useServerAction } from "@shared/components/global/hooks/use-server-action";
import { ErrorModal } from "@shared/components/molecules/modal/error";
import { EditorHeader } from "@shared/components/organisms/common/editor/header";
import { MarkdownPreview } from "@shared/components/organisms/common/editor/markdown-preview";
import { Article, UnvalidatedArticle } from "@shared/domains/articles";
import {
  articleMatter,
  ARTICLE_FRONTMATTER_TEMPLATE,
  updateFrontmatterTitle,
  updateFrontmatterTags,
  extractFrontmatterTitle,
  extractFrontmatterSlug,
  extractFrontmatterTags,
} from "@shared/components/global/matter";
import { TagSelect } from "@shared/components/molecules/select/tag";
import { Tag, TagIdentifier } from "@shared/domains/attributes/tag";
import { TagIcon } from "@shared/components/atoms/icon/tag";
import { CrossIcon } from "@shared/components/atoms/icon/cross";
import { ok } from "@shared/aspects/result";
import { LoadingOverlay } from "@shared/components/molecules/overlay/loading";

const MarkdownEditor = dynamic(
  () =>
    import("@shared/components/organisms/common/editor/markdown-editor").then(
      (module) => module.MarkdownEditor,
    ),
  {
    ssr: false,
    loading: () => (
      <div
        style={{
          width: "100%",
          height: "100%",
          minHeight: "100vh",
          backgroundColor: "var(--muted)",
          borderRadius: "var(--radius-md)",
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          color: "var(--muted-foreground)",
        }}
      >
        エディタを読み込み中...
      </div>
    ),
  },
);

export type Props = {
  initial?: Article;
  persist: (unvalidated: UnvalidatedArticle) => Promise<void>;
  tags: Tag[];
  uploadImage: (file: File | Blob, path: string) => Promise<string>;
};

export const ArticleEdit = (props: Props) => {
  const router = useRouter();

  const [identifier] = useState(() => props.initial?.identifier ?? ulid());
  const [title, setTitle] = useState<string>(props.initial?.title ?? "");
  const [slug, setSlug] = useState<string>(props.initial?.slug ?? "");
  const [content, setContent] = useState<string>(() => {
    const initialContent =
      props.initial?.content ?? ARTICLE_FRONTMATTER_TEMPLATE;
    const initialTags = props.initial?.tags ?? [];
    if (initialTags.length > 0) {
      return updateFrontmatterTags(initialContent, initialTags);
    }
    return initialContent;
  });
  const [status, setStatus] = useState<PublishStatus>(
    props.initial?.status ?? PublishStatus.DRAFT,
  );
  const [tags, setTags] = useState<TagIdentifier[]>(props.initial?.tags ?? []);

  const handleTitleChange = useCallback((newTitle: string) => {
    setTitle(newTitle);
    setContent((previousContent) =>
      updateFrontmatterTitle(previousContent, newTitle),
    );
  }, []);

  const handleContentChange = useCallback(
    (newContent: string) => {
      setContent(newContent);

      const frontmatterTitle = extractFrontmatterTitle(newContent);
      if (frontmatterTitle !== null && frontmatterTitle !== title) {
        setTitle(frontmatterTitle);
      }

      const frontmatterSlug = extractFrontmatterSlug(newContent);
      if (frontmatterSlug !== null && frontmatterSlug !== slug) {
        setSlug(frontmatterSlug);
      }

      const frontmatterTags = extractFrontmatterTags(newContent);
      if (frontmatterTags !== null) {
        const currentTagsString = tags.join(",");
        const newTagsString = frontmatterTags.join(",");
        if (currentTagsString !== newTagsString) {
          setTags(frontmatterTags as TagIdentifier[]);
        }
      }
    },
    [title, slug, tags],
  );

  const handleTagsChange = useCallback(
    (newTags: TagIdentifier[]) => {
      setTags(newTags);
      setContent((previousContent) =>
        updateFrontmatterTags(previousContent, newTags),
      );
    },
    [],
  );

  const { execute, error, isLoading, reset } = useServerAction(async () => {
    return articleMatter(content)
      .toAsync()
      .andThen(async (matter) => {
        const slug = props.initial?.slug ?? matter.data.slug;

        await props.persist({
          identifier,
          title,
          excerpt: matter.data.excerpt,
          content,
          status: status ?? PublishStatus.DRAFT,
          tags,
          slug,
          timeline: {
            createdAt: props.initial?.timeline.createdAt ?? new Date(),
            updatedAt: new Date(),
          },
        });

        return ok(slug);
      })
      .tap((slug) => {
        if (status === PublishStatus.PUBLISHED) {
          router.push(`/articles/${slug}`);
        }
      })
      .match({
        ok: () => {},
        err: (error) => {
          throw new Error(error.message);
        },
      });
  });

  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <EditorHeader
          title={title}
          onTitleChange={handleTitleChange}
          isPublished={status === PublishStatus.PUBLISHED}
          onPublishChange={(value) =>
            setStatus(value ? PublishStatus.PUBLISHED : PublishStatus.DRAFT)
          }
          persist={execute}
          isLoading={isLoading}
        />
      </div>

      <div className={styles.content}>
        <div className={styles.editor}>
          <MarkdownEditor
            value={content}
            onChange={handleContentChange}
            imageUpload={{
              enabled: slug.length > 0,
              contentType: "article",
              slug,
              uploadAction: props.uploadImage,
            }}
          />
          <div className={styles.tags}>
            <div className={styles["tags-header"]}>
              <span className={styles["tags-icon"]}>
                <TagIcon />
              </span>
              <span className={styles["tags-label"]}>タグ</span>
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
        </div>
        <div className={styles.preview}>
          <MarkdownPreview content={content} title={title} />
        </div>
      </div>
      {isLoading && <LoadingOverlay />}
      {error && (
        <ErrorModal message={error.message} onClose={reset} isOpen={!!error} />
      )}
    </div>
  );
};
