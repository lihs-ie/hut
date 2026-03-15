"use client";

import { useCallback, useRef, useEffect } from "react";
import { DropzoneOverlay } from "@shared/components/molecules/overlay/dropzone";
import { UploadStatus } from "@shared/components/molecules/upload/status";
import { ErrorModal } from "@shared/components/molecules/modal/error";
import { useImageUpload } from "@shared/components/global/hooks/use-image-upload";
import { useImageDropzone } from "@shared/components/global/hooks/use-image-dropzone";
import { ContentType } from "@shared/domains/common/image-upload";
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

  const valueRef = useRef(props.value);

  useEffect(() => {
    valueRef.current = props.value;
  }, [props.value]);

  const imageUploadHook = useImageUpload({
    uploadAction: imageUpload?.uploadAction ?? (async () => ""),
  });

  useEffect(() => {
    onUploadingChange?.(imageUploadHook.isUploading);
  }, [onUploadingChange, imageUploadHook.isUploading]);

  const { containerRef, insertText, focus } = useCodeMirror({
    value: props.value,
    onChange,
    placeholder: props.placeholder,
    onSave: props.onSave,
  });

  const replacePlaceholder = useCallback(
    (placeholderId: string, replacement: string) => {
      const placeholder = `![uploading...](placeholder-${placeholderId})`;
      const currentValue = valueRef.current;
      const newValue = currentValue.replace(placeholder, replacement);
      onChange(newValue);
    },
    [onChange],
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
