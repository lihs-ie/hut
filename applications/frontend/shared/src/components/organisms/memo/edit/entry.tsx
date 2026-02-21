"use client";

import { SimpleCard } from "@shared/components/atoms/card/simple";
import styles from "./entry.module.css";
import { useState, useCallback, useRef } from "react";
import { Textarea } from "@shared/components/atoms/input/textarea";
import { SimpleButton } from "@shared/components/atoms/button/simple";
import { useServerAction } from "@shared/components/global/hooks/use-server-action";
import { useImageUpload } from "@shared/components/global/hooks/use-image-upload";
import { useImageDropzone } from "@shared/components/global/hooks/use-image-dropzone";
import { MemoIdentifier, UnvalidatedEntry } from "@shared/domains/memo";
import { ImageIdentifier } from "@shared/domains/image";
import { extractImageUrls } from "@shared/domains/common/markdown";
import { ErrorModal } from "@shared/components/molecules/modal/error";
import { DropzoneOverlay } from "@shared/components/molecules/overlay/dropzone";
import { UploadStatus } from "@shared/components/molecules/upload/status";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import remarkBreaks from "remark-breaks";
import { Prism as SyntaxHighlighter } from "react-syntax-highlighter";
import { oneDark } from "react-syntax-highlighter/dist/esm/styles/prism";

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
  const textareaRef = useRef<HTMLTextAreaElement>(null);
  const imageUrlToIdentifierMap = useRef<Map<string, ImageIdentifier>>(new Map());

  const { execute, error, reset, isError } = useServerAction(props.persist);

  const imageUploadHook = useImageUpload({
    uploadAction: props.uploadAction,
  });

  const replacePlaceholder = useCallback(
    (placeholderId: string, replacement: string) => {
      const placeholder = `![uploading...](placeholder-${placeholderId})`;
      setValue((previous) => previous.replace(placeholder, replacement));
    },
    []
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

  const processFiles = useCallback(
    async (files: File[]) => {
      for (const file of files) {
        const placeholderId = `${Date.now()}-${Math.random().toString(36).slice(2)}`;
        setValue(
          (previous) =>
            previous + `\n![uploading...](placeholder-${placeholderId})\n`
        );
        await handleImageUpload(file, placeholderId);
      }
    },
    [handleImageUpload]
  );

  const { isDragOver, handlers, handlePaste } = useImageDropzone({
    onFilesDropped: processFiles,
  });

  return (
    <SimpleCard className={styles.container}>
      <div className={styles.main} {...handlers}>
        <div className={styles.body}>
          <div className={styles.tabs}>
            <button
              onClick={() => setActiveTab("markdown")}
              className={`${styles.tab} ${
                activeTab === Tab.MARKDOWN && styles["tab.is-active"]
              }`}
            >
              Markdown
            </button>
            <button
              onClick={() => setActiveTab("preview")}
              className={`${styles.tab} ${
                activeTab === Tab.PREVIEW && styles["tab.is-active"]
              }`}
            >
              Preview
            </button>
          </div>

          {activeTab === Tab.MARKDOWN ? (
            <div className={styles.editor}>
              <Textarea
                ref={textareaRef}
                value={value}
                onChange={handleValueChange}
                onPaste={handlePaste}
                placeholder="コメントを追加"
                className={styles.textarea}
              />
            </div>
          ) : (
            <div className={`${styles.preview} prose`}>
              {value ? (
                <ReactMarkdown
                  remarkPlugins={[remarkGfm, remarkBreaks]}
                  components={{
                    code(props) {
                      const { children, className, ...rest } = props;
                      const match = /language-(\w+)/.exec(className || "");
                      const isInline = !match && !className;

                      return isInline ? (
                        <code className={className} {...rest}>
                          {children}
                        </code>
                      ) : (
                        <SyntaxHighlighter
                          style={oneDark}
                          language={match ? match[1] : "text"}
                          PreTag="div"
                        >
                          {String(children).replace(/\n$/, "")}
                        </SyntaxHighlighter>
                      );
                    },
                  }}
                >
                  {value}
                </ReactMarkdown>
              ) : (
                <p className={styles["preview.empty"]}>
                  プレビューするコンテンツがありません
                </p>
              )}
            </div>
          )}
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
              setValue("");
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
