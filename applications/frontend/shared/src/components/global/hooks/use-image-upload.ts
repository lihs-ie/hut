"use client";

import { useState, useCallback, useRef } from "react";
import { ulid } from "ulid";
import { Result, ok, err } from "@shared/aspects/result";
import {
  ValidationError,
  UnexpectedError,
  validationError,
  unexpectedError,
} from "@shared/aspects/error";
import {
  validateImageFile,
  generateUploadPath,
  generateImageObjectName,
  ContentType,
} from "@shared/domains/common/image-upload";
import {
  ImageIdentifier,
  imageIdentifierSchema,
} from "@shared/domains/image";
import {
  compressImageToWebP,
  CompressedImage,
} from "./use-image-compression";

export type UploadState = {
  id: string;
  fileName: string;
  status: "compressing" | "uploading" | "completed" | "failed";
  progress: number;
  error?: string;
  url?: string;
};

export type PlaceholderInfo = {
  id: string;
  placeholder: string;
  altText: string;
};

export type UseImageUploadOptions = {
  uploadAction: (file: File | Blob, path: string) => Promise<string>;
};

export type UploadError = {
  message: string;
  uploadId?: string;
};

export type UploadResult = {
  url: string;
  placeholder: PlaceholderInfo;
  imageIdentifier: ImageIdentifier;
};

export type UseImageUploadReturn = {
  uploads: UploadState[];
  isUploading: boolean;
  error: UploadError | null;
  isError: boolean;
  clearError: () => void;
  uploadImage: (
    file: File,
    contentType: ContentType,
    reference: string,
  ) => Promise<
    Result<
      UploadResult,
      ValidationError | UnexpectedError
    >
  >;
  cancelUpload: (id: string) => void;
  clearCompleted: () => void;
};

export const useImageUpload = (
  options: UseImageUploadOptions,
): UseImageUploadReturn => {
  const [uploads, setUploads] = useState<UploadState[]>([]);
  const [error, setError] = useState<UploadError | null>(null);
  const abortControllers = useRef<Map<string, AbortController>>(new Map());

  const clearError = useCallback(() => {
    setError(null);
  }, []);

  const updateUploadState = useCallback(
    (id: string, update: Partial<UploadState>) => {
      setUploads((previous) =>
        previous.map((upload) =>
          upload.id === id ? { ...upload, ...update } : upload,
        ),
      );
    },
    [],
  );

  const uploadImage = useCallback(
    async (
      file: File,
      contentType: ContentType,
      reference: string,
    ): Promise<
      Result<
        UploadResult,
        ValidationError | UnexpectedError
      >
    > => {
      const uploadId = ulid();
      const abortController = new AbortController();
      abortControllers.current.set(uploadId, abortController);

      const placeholder: PlaceholderInfo = {
        id: uploadId,
        placeholder: `![uploading...](placeholder-${uploadId})`,
        altText: file.name,
      };

      const initialState: UploadState = {
        id: uploadId,
        fileName: file.name,
        status: "compressing",
        progress: 0,
      };
      setUploads((previous) => [...previous, initialState]);

      try {
        const validationResult = validateImageFile(file);
        if (validationResult.isErr) {
          const validationErr = validationResult.unwrapError();
          updateUploadState(uploadId, {
            status: "failed",
            error: validationErr.description,
          });
          setError({ message: validationErr.description, uploadId });
          return err(validationErr);
        }

        if (abortController.signal.aborted) {
          return err(
            validationError("cancelled", "アップロードがキャンセルされました"),
          );
        }

        updateUploadState(uploadId, { status: "compressing", progress: 25 });

        type CompressionOutcome =
          | { ok: true; value: CompressedImage }
          | { ok: false; error: ValidationError };

        const compressionOutcome: CompressionOutcome =
          await compressImageToWebP(file).match<CompressionOutcome>({
            ok: (value) => ({ ok: true, value }),
            err: (error) => ({ ok: false, error }),
          });

        if (!compressionOutcome.ok) {
          updateUploadState(uploadId, {
            status: "failed",
            error: compressionOutcome.error.description,
          });
          setError({ message: compressionOutcome.error.description, uploadId });
          return err(compressionOutcome.error);
        }

        const compressed = compressionOutcome.value;

        if (abortController.signal.aborted) {
          return err(
            validationError("cancelled", "アップロードがキャンセルされました"),
          );
        }

        updateUploadState(uploadId, { status: "uploading", progress: 50 });

        const imageIdentifier = imageIdentifierSchema.parse(uploadId);
        const extension = compressed.mimeType === "image/gif" ? "gif" : "webp";
        const objectName = generateImageObjectName(uploadId, extension);
        const path = generateUploadPath(contentType, reference, objectName);

        try {
          const url = await options.uploadAction(compressed.blob, path);

          updateUploadState(uploadId, {
            status: "completed",
            progress: 100,
            url,
          });

          abortControllers.current.delete(uploadId);

          return ok({ url, placeholder, imageIdentifier });
        } catch (uploadErr) {
          const message =
            uploadErr instanceof Error
              ? uploadErr.message
              : "アップロードに失敗しました";

          updateUploadState(uploadId, {
            status: "failed",
            error: message,
          });
          setError({ message, uploadId });

          return err(unexpectedError(message, uploadErr));
        }
      } catch (unexpectedErr) {
        const message =
          unexpectedErr instanceof Error
            ? unexpectedErr.message
            : "予期しないエラーが発生しました";

        updateUploadState(uploadId, {
          status: "failed",
          error: message,
        });
        setError({ message, uploadId });

        return err(unexpectedError(message, unexpectedErr));
      }
    },
    [options, updateUploadState],
  );

  const cancelUpload = useCallback(
    (id: string) => {
      const controller = abortControllers.current.get(id);
      if (controller) {
        controller.abort();
        abortControllers.current.delete(id);
      }
      updateUploadState(id, {
        status: "failed",
        error: "キャンセルされました",
      });
    },
    [updateUploadState],
  );

  const clearCompleted = useCallback(() => {
    setUploads((previous) =>
      previous.filter(
        (upload) => upload.status !== "completed" && upload.status !== "failed",
      ),
    );
  }, []);

  const isUploading = uploads.some(
    (upload) =>
      upload.status === "compressing" || upload.status === "uploading",
  );

  const isError = error !== null;

  return {
    uploads,
    isUploading,
    error,
    isError,
    clearError,
    uploadImage,
    cancelUpload,
    clearCompleted,
  };
};
