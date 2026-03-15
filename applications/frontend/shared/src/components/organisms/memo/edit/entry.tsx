"use client";

import { SimpleCard } from "@shared/components/atoms/card/simple";
import styles from "./entry.module.css";
import { useState, useCallback, useRef, useEffect } from "react";
import { SimpleButton } from "@shared/components/atoms/button/simple";
import { useServerAction } from "@shared/components/global/hooks/use-server-action";
import { useImageUpload } from "@shared/components/global/hooks/use-image-upload";
import { useImageDropzone } from "@shared/components/global/hooks/use-image-dropzone";
import { MemoIdentifier, UnvalidatedEntry } from "@shared/domains/memo";
import { ImageIdentifier } from "@shared/domains/image";
import { extractImageUrls } from "@shared/domains/common/markdown";
import { SUPPORTED_IMAGE_MIME_TYPES, SupportedImageMimeType } from "@shared/domains/common/image-upload";
import { ErrorModal } from "@shared/components/molecules/modal/error";
import { DropzoneOverlay } from "@shared/components/molecules/overlay/dropzone";
import { UploadStatus } from "@shared/components/molecules/upload/status";
import { useCodeMirror } from "@shared/components/organisms/common/editor/use-codemirror";
import { EntryPreview } from "./preview";

export type Props = {
  persist: (unvalidated: UnvalidatedEntry, slug: string, images: ImageIdentifier[]) => Promise<void>;
  slug: string;
  memoIdentifier: MemoIdentifier;
  uploadAction: (file: File | Blob, path: string) => Promise<string>;
};

const Tab = {
  MARKDOWN: "markdown",
  PREVIEW: "preview",
} as const;

type Tab = (typeof Tab)[keyof typeof Tab];

export const EntryEditor = (props: Props) => {
  const [activeTab, setActiveTab] = useState<Tab>(Tab.MARKDOWN);
  const [value, setValue] = useState("");
  const [images, setImages] = useState<ImageIdentifier[]>([]);
  const imageUrlToIdentifierMap = useRef<Map<string, ImageIdentifier>>(new Map());

  const { execute, error, reset, isError } = useServerAction(props.persist);

  const imageUploadHook = useImageUpload({
    uploadAction: props.uploadAction,
  });

  const handleValueChange = useCallback((newValue: string) => {
    setValue(newValue);
    const currentUrls = extractImageUrls(newValue);
    const removedIdentifiers: ImageIdentifier[] = [];
    imageUrlToIdentifierMap.current.forEach((identifier, url) => {
      if (!currentUrls.has(url)) {
        removedIdentifiers.push(identifier);
        imageUrlToIdentifierMap.current.delete(url);
      }
    });
    if (removedIdentifiers.length > 0) {
      setImages((previous) => previous.filter((id) => !removedIdentifiers.includes(id)));
    }
  }, []);

  const processFilesRef = useRef<((files: File[]) => Promise<void>) | null>(null);

  const handlePasteFromEditor = useCallback((event: ClipboardEvent) => {
    const items = event.clipboardData?.items;
    if (!items) return;
    const imageFiles: File[] = [];
    for (const item of Array.from(items)) {
      if (SUPPORTED_IMAGE_MIME_TYPES.includes(item.type as SupportedImageMimeType)) {
        const file = item.getAsFile();
        if (file) imageFiles.push(file);
      }
    }
    if (imageFiles.length > 0) {
      event.preventDefault();
      processFilesRef.current?.(imageFiles);
    }
  }, []);

  const handleDropFromEditor = useCallback((files: File[]) => {
    processFilesRef.current?.(files);
  }, []);

  const { containerRef, insertText, replaceText } = useCodeMirror({
    value,
    onChange: handleValueChange,
    placeholder: "コメントを追加",
    onPaste: handlePasteFromEditor,
    onDrop: handleDropFromEditor,
  });

  const replacePlaceholder = useCallback(
    (placeholderId: string, replacement: string) => {
      const placeholder = `![uploading...](placeholder-${placeholderId})`;
      replaceText(placeholder, replacement);
    },
    [replaceText]
  );

  const handleImageUpload = useCallback(
    async (file: File, placeholderId: string) => {
      const result = await imageUploadHook.uploadImage(
        file,
        "memo",
        props.memoIdentifier
      );

      result.match({
        ok: ({ url, placeholder, imageIdentifier }) => {
          replacePlaceholder(
            placeholderId,
            `![${placeholder.altText}](${url})`
          );
          imageUrlToIdentifierMap.current.set(url, imageIdentifier);
          setImages((previous) => [...previous, imageIdentifier]);
        },
        err: (uploadError) => {
          const message =
            "description" in uploadError
              ? uploadError.description
              : uploadError.message;
          replacePlaceholder(
            placeholderId,
            `<!-- 画像アップロード失敗: ${message} -->`
          );
        },
      });
    },
    [props.memoIdentifier, imageUploadHook, replacePlaceholder]
  );

  const processFiles = useCallback(
    async (files: File[]) => {
      for (const file of files) {
        const placeholderId = `${Date.now()}-${Math.random().toString(36).slice(2)}`;
        insertText(`\n![uploading...](placeholder-${placeholderId})\n`);
        await handleImageUpload(file, placeholderId);
      }
    },
    [handleImageUpload, insertText]
  );

  useEffect(() => {
    processFilesRef.current = processFiles;
  }, [processFiles]);

  const { isDragOver, handlers } = useImageDropzone({
    onFilesDropped: processFiles,
  });

  return (
    <SimpleCard className={styles.container}>
      <div className={styles.main} {...handlers}>
        <div className={styles.body}>
          <div className={styles.tabs}>
            <button
              type="button"
              onClick={() => setActiveTab("markdown")}
              className={`${styles.tab} ${
                activeTab === Tab.MARKDOWN ? styles.active : ""
              }`}
            >
              Markdown
            </button>
            <button
              type="button"
              onClick={() => setActiveTab("preview")}
              className={`${styles.tab} ${
                activeTab === Tab.PREVIEW ? styles.active : ""
              }`}
            >
              Preview
            </button>
          </div>

          <div
            className={styles.editor}
            style={{ display: activeTab === Tab.MARKDOWN ? "block" : "none" }}
          >
            <div ref={containerRef} className={styles["codemirror-container"]} />
          </div>
          <div
            className={`${styles.preview} prose`}
            style={{ display: activeTab === Tab.PREVIEW ? "block" : "none" }}
          >
            {value ? (
              <EntryPreview value={value} />
            ) : (
              <p className={styles.empty}>
                プレビューするコンテンツがありません
              </p>
            )}
          </div>
        </div>

        <DropzoneOverlay isActive={isDragOver} />
        <UploadStatus
          uploads={imageUploadHook.uploads}
          onCancel={imageUploadHook.cancelUpload}
          onClear={imageUploadHook.clearCompleted}
        />
      </div>

      <div className={styles.button}>
        <SimpleButton
          type="button"
          disabled={imageUploadHook.isUploading}
          onClick={async () => {
            try {
              await execute({ text: value, createdAt: new Date() }, props.slug, images);
              handleValueChange("");
              setImages([]);
              imageUrlToIdentifierMap.current.clear();
            } catch {
            }
          }}
        >
          {imageUploadHook.isUploading ? "画像アップロード中..." : "投稿する"}
        </SimpleButton>
      </div>

      <ErrorModal
        isOpen={isError}
        onClose={reset}
        title="コメントの追加に失敗しました"
        message={error?.message ?? "不明なエラーが発生しました"}
        details={error?.details}
      />

      <ErrorModal
        isOpen={imageUploadHook.isError}
        onClose={imageUploadHook.clearError}
        title="画像アップロードエラー"
        message={imageUploadHook.error?.message ?? "不明なエラーが発生しました"}
      />
    </SimpleCard>
  );
};
