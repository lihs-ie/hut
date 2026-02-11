"use client";

import { useCallback, useRef, useEffect } from "react";
import { DropzoneOverlay } from "@shared/components/molecules/overlay/dropzone";
import { UploadStatus } from "@shared/components/molecules/upload/status";
import { ErrorModal } from "@shared/components/molecules/modal/error";
import { useImageUpload } from "@shared/components/global/hooks/use-image-upload";
import { useImageDropzone } from "@shared/components/global/hooks/use-image-dropzone";
import { ContentType } from "@shared/domains/common/image-upload";
import styles from "./markdown-editor.module.css";

export type ImageUploadConfig = {
  enabled: boolean;
  contentType: ContentType;
  slug: string;
  uploadAction: (file: File | Blob, path: string) => Promise<string>;
};

export type Props = {
  value: string;
  onChange: (value: string) => void;
  placeholder?: string;
  imageUpload?: ImageUploadConfig;
};

export const MarkdownEditor = (props: Props) => {
  const textareaRef = useRef<HTMLTextAreaElement>(null);
  const valueRef = useRef(props.value);

  useEffect(() => {
    valueRef.current = props.value;
  }, [props.value]);

  const imageUploadHook = useImageUpload({
    uploadAction: props.imageUpload?.uploadAction ?? (async () => ""),
  });

  const insertTextAtCursor = useCallback(
    (text: string) => {
      const textarea = textareaRef.current;
      if (!textarea) return;

      const start = textarea.selectionStart;
      const end = textarea.selectionEnd;
      const currentValue = valueRef.current;
      const newValue =
        currentValue.substring(0, start) + text + currentValue.substring(end);

      props.onChange(newValue);

      requestAnimationFrame(() => {
        textarea.selectionStart = textarea.selectionEnd = start + text.length;
        textarea.focus();
      });
    },
    [props],
  );

  const replacePlaceholder = useCallback(
    (placeholderId: string, replacement: string) => {
      const placeholder = `![uploading...](placeholder-${placeholderId})`;
      const currentValue = valueRef.current;
      const newValue = currentValue.replace(placeholder, replacement);
      props.onChange(newValue);
    },
    [props],
  );

  const handleImageUpload = useCallback(
    async (file: File, placeholderId: string) => {
      if (!props.imageUpload?.enabled) return;

      const result = await imageUploadHook.uploadImage(
        file,
        props.imageUpload.contentType,
        props.imageUpload.slug,
      );

      result.match({
        ok: ({ url, placeholder }) => {
          replacePlaceholder(
            placeholderId,
            `![${placeholder.altText}](${url})`,
          );
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
    [props.imageUpload, imageUploadHook, replacePlaceholder],
  );

  const processFiles = useCallback(
    async (files: File[]) => {
      for (const file of files) {
        const placeholderId = `${Date.now()}-${Math.random().toString(36).slice(2)}`;
        insertTextAtCursor(`\n![uploading...](placeholder-${placeholderId})\n`);
        await handleImageUpload(file, placeholderId);
      }
    },
    [handleImageUpload, insertTextAtCursor],
  );

  const { isDragOver, handlers, handlePaste } = useImageDropzone({
    enabled: props.imageUpload?.enabled,
    onFilesDropped: processFiles,
  });

  const getLineRange = useCallback(
    (start: number, end: number) => {
      const lineStart = props.value.lastIndexOf("\n", start - 1) + 1;
      const lineEndIndex = props.value.indexOf("\n", end);
      const lineEnd = lineEndIndex === -1 ? props.value.length : lineEndIndex;
      return { lineStart, lineEnd };
    },
    [props.value],
  );

  const wrapSelection = useCallback(
    (textarea: HTMLTextAreaElement, wrapper: string, placeholder: string) => {
      const start = textarea.selectionStart;
      const end = textarea.selectionEnd;
      const selectedText = props.value.substring(start, end);

      if (selectedText) {
        const isWrapped =
          selectedText.startsWith(wrapper) && selectedText.endsWith(wrapper);
        if (isWrapped) {
          const unwrapped = selectedText.slice(wrapper.length, -wrapper.length);
          const newValue =
            props.value.substring(0, start) +
            unwrapped +
            props.value.substring(end);
          props.onChange(newValue);
          requestAnimationFrame(() => {
            textarea.selectionStart = start;
            textarea.selectionEnd = start + unwrapped.length;
          });
        } else {
          const wrapped = `${wrapper}${selectedText}${wrapper}`;
          const newValue =
            props.value.substring(0, start) +
            wrapped +
            props.value.substring(end);
          props.onChange(newValue);
          requestAnimationFrame(() => {
            textarea.selectionStart = start;
            textarea.selectionEnd = start + wrapped.length;
          });
        }
      } else {
        const wrapped = `${wrapper}${placeholder}${wrapper}`;
        const newValue =
          props.value.substring(0, start) +
          wrapped +
          props.value.substring(end);
        props.onChange(newValue);
        requestAnimationFrame(() => {
          textarea.selectionStart = start + wrapper.length;
          textarea.selectionEnd = start + wrapper.length + placeholder.length;
        });
      }
    },
    [props],
  );

  const handleKeyDown = useCallback(
    (event: React.KeyboardEvent<HTMLTextAreaElement>) => {
      const textarea = event.currentTarget;
      const start = textarea.selectionStart;
      const end = textarea.selectionEnd;
      const spaces = "    ";

      if (event.key === "Tab" && !event.shiftKey) {
        event.preventDefault();

        if (start === end) {
          const newValue =
            props.value.substring(0, start) +
            spaces +
            props.value.substring(end);
          props.onChange(newValue);
          requestAnimationFrame(() => {
            textarea.selectionStart = textarea.selectionEnd =
              start + spaces.length;
          });
        } else {
          const { lineStart, lineEnd } = getLineRange(start, end);
          const selectedLines = props.value.substring(lineStart, lineEnd);
          const indentedLines = selectedLines
            .split("\n")
            .map((line) => spaces + line)
            .join("\n");
          const newValue =
            props.value.substring(0, lineStart) +
            indentedLines +
            props.value.substring(lineEnd);
          props.onChange(newValue);
          const lineCount = selectedLines.split("\n").length;
          requestAnimationFrame(() => {
            textarea.selectionStart = start + spaces.length;
            textarea.selectionEnd = end + spaces.length * lineCount;
          });
        }
      }

      if (event.key === "Tab" && event.shiftKey) {
        event.preventDefault();

        if (start === end) {
          const lineStart = props.value.lastIndexOf("\n", start - 1) + 1;
          const lineContent = props.value.substring(lineStart);
          const leadingSpaces = lineContent.match(/^[ ]+/);
          if (leadingSpaces) {
            const spacesToRemove = Math.min(leadingSpaces[0].length, 4);
            const newValue =
              props.value.substring(0, lineStart) +
              props.value.substring(lineStart + spacesToRemove);
            props.onChange(newValue);
            requestAnimationFrame(() => {
              textarea.selectionStart = textarea.selectionEnd = Math.max(
                lineStart,
                start - spacesToRemove,
              );
            });
          }
        } else {
          const { lineStart, lineEnd } = getLineRange(start, end);
          const selectedLines = props.value.substring(lineStart, lineEnd);
          let totalRemoved = 0;
          let firstLineRemoved = 0;
          const unindentedLines = selectedLines
            .split("\n")
            .map((line, index) => {
              const leadingSpaces = line.match(/^[ ]+/);
              if (leadingSpaces) {
                const spacesToRemove = Math.min(leadingSpaces[0].length, 4);
                totalRemoved += spacesToRemove;
                if (index === 0) firstLineRemoved = spacesToRemove;
                return line.substring(spacesToRemove);
              }
              return line;
            })
            .join("\n");
          const newValue =
            props.value.substring(0, lineStart) +
            unindentedLines +
            props.value.substring(lineEnd);
          props.onChange(newValue);
          requestAnimationFrame(() => {
            textarea.selectionStart = Math.max(
              lineStart,
              start - firstLineRemoved,
            );
            textarea.selectionEnd = end - totalRemoved;
          });
        }
      }

      if (event.key === "x" && (event.ctrlKey || event.metaKey)) {
        if (start === end) {
          event.preventDefault();

          const lineStart = props.value.lastIndexOf("\n", start - 1) + 1;
          const lineEndIndex = props.value.indexOf("\n", start);
          const actualLineEnd =
            lineEndIndex === -1 ? props.value.length : lineEndIndex;

          const line = props.value.substring(lineStart, actualLineEnd);
          const includeNewline = lineEndIndex !== -1;
          const cutText = includeNewline ? line + "\n" : line;

          navigator.clipboard.writeText(cutText);

          const newValue =
            props.value.substring(0, lineStart) +
            props.value.substring(
              includeNewline ? actualLineEnd + 1 : actualLineEnd,
            );

          props.onChange(newValue);

          requestAnimationFrame(() => {
            textarea.selectionStart = textarea.selectionEnd = lineStart;
          });
        }
      }

      if (event.key === "b" && (event.ctrlKey || event.metaKey)) {
        event.preventDefault();
        wrapSelection(textarea, "**", "太字");
      }

      if (event.key === "i" && (event.ctrlKey || event.metaKey)) {
        event.preventDefault();
        wrapSelection(textarea, "*", "イタリック");
      }

      if (event.key === "k" && (event.ctrlKey || event.metaKey)) {
        event.preventDefault();
        const selectedText = props.value.substring(start, end);

        if (selectedText) {
          const linkText = `[${selectedText}](url)`;
          const newValue =
            props.value.substring(0, start) +
            linkText +
            props.value.substring(end);
          props.onChange(newValue);
          requestAnimationFrame(() => {
            textarea.selectionStart = start + selectedText.length + 3;
            textarea.selectionEnd = start + selectedText.length + 6;
          });
        } else {
          const linkText = "[リンクテキスト](url)";
          const newValue =
            props.value.substring(0, start) +
            linkText +
            props.value.substring(end);
          props.onChange(newValue);
          requestAnimationFrame(() => {
            textarea.selectionStart = start + 1;
            textarea.selectionEnd = start + 7;
          });
        }
      }
    },
    [props, getLineRange, wrapSelection],
  );

  return (
    <div className={styles.container} {...handlers}>
      <div className={styles["editor-area"]}>
        <textarea
          ref={textareaRef}
          value={props.value}
          onChange={(event) => props.onChange(event.target.value)}
          onKeyDown={handleKeyDown}
          onPaste={handlePaste}
          placeholder={props.placeholder || "マークダウンで記述"}
          className={styles.textarea}
        />
      </div>

      {props.imageUpload?.enabled && (
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
