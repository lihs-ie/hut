"use client";

import styles from "./edit.module.css";
import dynamic from "next/dynamic";
import { useCallback, useRef, useState } from "react";
import { ulid } from "ulid";
import { PublishStatus } from "@shared/domains/common";
import { useServerAction } from "@shared/components/global/hooks/use-server-action";
import { ErrorModal } from "@shared/components/molecules/modal/error";
import { EditorHeader } from "@shared/components/organisms/common/editor/header";
import { MarkdownPreview } from "@shared/components/organisms/common/editor/markdown-preview";
import { Chapter, UnvalidatedChapter } from "@shared/domains/series/chapter";
import { LoadingOverlay } from "@shared/components/molecules/overlay/loading";
import { useToast } from "@shared/components/molecules/toast";
import { ImageIdentifier } from "@shared/domains/image";
import { extractImageUrls } from "@shared/domains/common/markdown";

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
  initial?: Chapter;
  persist: (unvalidated: UnvalidatedChapter) => Promise<void>;
  uploadImage: (file: File | Blob, path: string) => Promise<string>;
  seriesSlug: string;
};

export const ChapterEditOrganism = (props: Props) => {
  const { showToast } = useToast();

  const [identifier] = useState(() => props.initial?.identifier ?? ulid());
  const [title, setTitle] = useState<string>(props.initial?.title ?? "");
  const [content, setContent] = useState<string>(
    props.initial?.content ?? "",
  );
  const [status, setStatus] = useState<PublishStatus>(
    props.initial?.status ?? PublishStatus.DRAFT,
  );
  const [images, setImages] = useState<ImageIdentifier[]>(
    props.initial?.images ?? [],
  );
  const [isEditorUploading, setIsEditorUploading] = useState(false);
  const imageUrlToIdentifierMap = useRef<Map<string, ImageIdentifier>>(
    new Map(),
  );

  const handleContentChange = useCallback(
    (newContent: string) => {
      setContent(newContent);

      const currentUrls = extractImageUrls(newContent);
      const removedIdentifiers: ImageIdentifier[] = [];
      imageUrlToIdentifierMap.current.forEach((imageIdentifier, url) => {
        if (!currentUrls.has(url)) {
          removedIdentifiers.push(imageIdentifier);
          imageUrlToIdentifierMap.current.delete(url);
        }
      });
      if (removedIdentifiers.length > 0) {
        setImages((previous) =>
          previous.filter((id) => !removedIdentifiers.includes(id)),
        );
      }
    },
    [],
  );

  const handleImageUploaded = useCallback(
    (imageIdentifier: ImageIdentifier, url: string) => {
      imageUrlToIdentifierMap.current.set(url, imageIdentifier);
      setImages((previous) => [...previous, imageIdentifier]);
    },
    [],
  );

  const { execute, error, isLoading, reset } = useServerAction(
    async () => {
      await props.persist({
        identifier,
        title,
        slug: props.initial?.slug ?? props.seriesSlug,
        content: content.trim() || title,
        images,
        status: status ?? PublishStatus.DRAFT,
        timeline: {
          createdAt: props.initial?.timeline.createdAt ?? new Date(),
          updatedAt: new Date(),
        },
      });
    },
    {
      onSuccess: () =>
        showToast(
          props.initial ? "チャプターを更新しました" : "チャプターを作成しました",
        ),
    },
  );

  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <EditorHeader
          title={title}
          onTitleChange={setTitle}
          isPublished={status === PublishStatus.PUBLISHED}
          onPublishChange={(value) =>
            setStatus(value ? PublishStatus.PUBLISHED : PublishStatus.DRAFT)
          }
          persist={execute}
          isLoading={isLoading}
          isUploading={isEditorUploading}
        />
      </div>

      <div className={styles.content}>
        <div className={styles.editor}>
          <MarkdownEditor
            value={content}
            onChange={handleContentChange}
            imageUpload={{
              enabled: true,
              contentType: "chapter",
              reference: identifier,
              uploadAction: props.uploadImage,
            }}
            onImageUploaded={handleImageUploaded}
            onUploadingChange={setIsEditorUploading}
          />
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
