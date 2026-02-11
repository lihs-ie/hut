"use client";

import { AsyncResult, ok, err, asyncResult } from "@shared/aspects/result";
import { ValidationError, validationError } from "@shared/aspects/error";
import {
  SupportedImageMimeType,
  IMAGE_COMPRESSION_CONFIG,
} from "@shared/domains/common/image-upload";

export type CompressedImage = {
  blob: Blob;
  originalSize: number;
  compressedSize: number;
  width: number;
  height: number;
  mimeType: string;
};

export type CompressionOptions = {
  maxWidth: number;
  maxHeight: number;
  quality: number;
};

export const DEFAULT_COMPRESSION_OPTIONS: CompressionOptions = {
  maxWidth: IMAGE_COMPRESSION_CONFIG.maxWidth,
  maxHeight: IMAGE_COMPRESSION_CONFIG.maxHeight,
  quality: IMAGE_COMPRESSION_CONFIG.quality,
};

export const calculateResizedDimensions = (
  originalWidth: number,
  originalHeight: number,
  maxWidth: number,
  maxHeight: number,
): { width: number; height: number } => {
  let width = originalWidth;
  let height = originalHeight;

  if (width > maxWidth) {
    height = Math.round((height * maxWidth) / width);
    width = maxWidth;
  }

  if (height > maxHeight) {
    width = Math.round((width * maxHeight) / height);
    height = maxHeight;
  }

  return { width, height };
};

export const getCompressedFileName = (
  originalName: string,
  mimeType: SupportedImageMimeType,
): string => {
  if (mimeType === "image/gif") {
    return originalName;
  }

  const nameWithoutExtension = originalName.replace(/\.[^.]+$/, "");
  return `${nameWithoutExtension}.webp`;
};

const loadImageFromFile = (file: File): Promise<HTMLImageElement> => {
  return new Promise((resolve, reject) => {
    const image = new Image();
    const url = URL.createObjectURL(file);

    image.onload = () => {
      URL.revokeObjectURL(url);
      resolve(image);
    };

    image.onerror = () => {
      URL.revokeObjectURL(url);
      reject(new Error("画像の読み込みに失敗しました"));
    };

    image.src = url;
  });
};

const canvasToBlob = (
  canvas: HTMLCanvasElement,
  mimeType: string,
  quality: number,
): Promise<Blob> => {
  return new Promise((resolve, reject) => {
    canvas.toBlob(
      (blob) => {
        if (blob) {
          resolve(blob);
        } else {
          reject(new Error("Blob変換に失敗しました"));
        }
      },
      mimeType,
      quality,
    );
  });
};

export const compressImageToWebP = (
  file: File,
  options: CompressionOptions = DEFAULT_COMPRESSION_OPTIONS,
): AsyncResult<CompressedImage, ValidationError> => {
  return asyncResult(
    (async () => {
      if (file.type === "image/gif") {
        return ok<CompressedImage, ValidationError>({
          blob: file,
          originalSize: file.size,
          compressedSize: file.size,
          width: 0,
          height: 0,
          mimeType: "image/gif",
        });
      }

      try {
        const image = await loadImageFromFile(file);

        const { width, height } = calculateResizedDimensions(
          image.width,
          image.height,
          options.maxWidth,
          options.maxHeight,
        );

        const canvas = document.createElement("canvas");
        canvas.width = width;
        canvas.height = height;

        const context = canvas.getContext("2d");
        if (!context) {
          return err<CompressedImage, ValidationError>(
            validationError("canvas", "Canvas 2D contextの取得に失敗しました"),
          );
        }

        context.drawImage(image, 0, 0, width, height);

        const blob = await canvasToBlob(canvas, "image/webp", options.quality);

        return ok<CompressedImage, ValidationError>({
          blob,
          originalSize: file.size,
          compressedSize: blob.size,
          width,
          height,
          mimeType: "image/webp",
        });
      } catch (error) {
        const message =
          error instanceof Error ? error.message : "画像の圧縮に失敗しました";
        return err<CompressedImage, ValidationError>(
          validationError("compression", message),
        );
      }
    })(),
  );
};
