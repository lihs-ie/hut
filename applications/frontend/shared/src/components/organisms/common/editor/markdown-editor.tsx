"use client";

import { useCallback, useRef, useEffect } from "react";
import { DropzoneOverlay } from "@shared/components/molecules/overlay/dropzone";
import { UploadStatus } from "@shared/components/molecules/upload/status";
import { ErrorModal } from "@shared/components/molecules/modal/error";
import { useImageUpload } from "@shared/components/global/hooks/use-image-upload";
import { useImageDropzone } from "@shared/components/global/hooks/use-image-dropzone";
import { ContentType, SUPPORTED_IMAGE_MIME_TYPES, SupportedImageMimeType } from "@shared/domains/common/image-upload";
import { ImageIdentifier } from "@shared/domains/image";
import { useCodeMirror } from "./use-codemirror";
import { EditorToolbar } from "./toolbar";
import styles from "./markdown-editor.module.css";

export type ImageUploadConfig = {
  enabled: boolean;
  contentType: ContentType;
  reference: string;
  uploadAction: (file: File | Blob, path: string) => Promise<string>;
};

export type Props = {
  value: string;
  onChange: (value: string) => void;
  placeholder?: string;
  imageUpload?: ImageUploadConfig;
  onImageUploaded?: (imageIdentifier: ImageIdentifier, url: string) => void;
  onUploadingChange?: (uploading: boolean) => void;
  onSave?: () => void;
};

export const MarkdownEditor = (props: Props) => {
  const onChange = props.onChange;
  const onUploadingChange = props.onUploadingChange;
  const onImageUploaded = props.onImageUploaded;
  const imageUpload = props.imageUpload;

  const imageUploadHook = useImageUpload({
    uploadAction: imageUpload?.uploadAction ?? (async () => ""),
  });

  useEffect(() => {
    onUploadingChange?.(imageUploadHook.isUploading);
  }, [onUploadingChange, imageUploadHook.isUploading]);

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

  const { containerRef, insertText, replaceText, focus } = useCodeMirror({
    value: props.value,
    onChange,
    placeholder: props.placeholder,
    onSave: props.onSave,
    onPaste: imageUpload?.enabled ? handlePasteFromEditor : undefined,
    onDrop: imageUpload?.enabled ? handleDropFromEditor : undefined,
  });

  const replacePlaceholder = useCallback(
    (placeholderId: string, replacement: string) => {
      const placeholder = `![uploading...](placeholder-${placeholderId})`;
      replaceText(placeholder, replacement);
    },
    [replaceText],
  );

  const handleImageUpload = useCallback(
    async (file: File, placeholderId: string) => {
      if (!imageUpload?.enabled) return;

      const result = await imageUploadHook.uploadImage(
        file,
        imageUpload.contentType,
        imageUpload.reference,
      );

      result.match({
        ok: ({ url, placeholder, imageIdentifier }) => {
          replacePlaceholder(
            placeholderId,
            `![${placeholder.altText}](${url})`,
          );
          onImageUploaded?.(imageIdentifier, url);
        },
        err: (uploadError) => {
          const message =
            "description" in uploadError
              ? uploadError.description
              : uploadError.message;
          replacePlaceholder(
            placeholderId,
            `<!-- 画像アップロード失敗: ${message} -->`,
          );
        },
      });
    },
    [imageUpload, onImageUploaded, imageUploadHook, replacePlaceholder],
  );

  const processFiles = useCallback(
    async (files: File[]) => {
      for (const file of files) {
        const placeholderId = `${Date.now()}-${Math.random().toString(36).slice(2)}`;
        insertText(`\n![uploading...](placeholder-${placeholderId})\n`);
        await handleImageUpload(file, placeholderId);
      }
    },
    [handleImageUpload, insertText],
  );

  useEffect(() => {
    processFilesRef.current = processFiles;
  }, [processFiles]);

  const handleImageButtonClick = useCallback(() => {
    const input = document.createElement("input");
    input.type = "file";
    input.accept = "image/*";
    input.multiple = true;
    input.onchange = async (event) => {
      const target = event.target as HTMLInputElement;
      if (target.files) {
        await processFiles(Array.from(target.files));
      }
    };
    input.click();
  }, [processFiles]);

  const handleBold = useCallback(() => {
    focus();
  }, [focus]);

  const handleItalic = useCallback(() => {
    focus();
  }, [focus]);

  const handleLink = useCallback(() => {
    insertText("[リンクテキスト](url)");
  }, [insertText]);

  const { isDragOver, handlers } = useImageDropzone({
    enabled: imageUpload?.enabled,
    onFilesDropped: processFiles,
  });

  return (
    <div className={styles.container} {...handlers}>
      <EditorToolbar
        onBold={handleBold}
        onItalic={handleItalic}
        onLink={handleLink}
        onImage={imageUpload?.enabled ? handleImageButtonClick : undefined}
      />
      <div className={styles["editor-area"]}>
        <div ref={containerRef} className={styles["codemirror-container"]} />
      </div>

      {imageUpload?.enabled && (
        <>
          <DropzoneOverlay isActive={isDragOver} />
          <UploadStatus
            uploads={imageUploadHook.uploads}
            onCancel={imageUploadHook.cancelUpload}
            onClear={imageUploadHook.clearCompleted}
          />
        </>
      )}

      <ErrorModal
        isOpen={imageUploadHook.isError}
        onClose={imageUploadHook.clearError}
        title="画像アップロードエラー"
        message={imageUploadHook.error?.message ?? "不明なエラーが発生しました"}
      />
    </div>
  );
};
