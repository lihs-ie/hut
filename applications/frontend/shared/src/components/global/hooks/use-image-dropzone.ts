"use client";

import { useState, useCallback, DragEvent, ClipboardEvent } from "react";
import {
  SUPPORTED_IMAGE_MIME_TYPES,
  SupportedImageMimeType,
} from "@shared/domains/common/image-upload";

export type UseImageDropzoneOptions = {
  enabled?: boolean;
  onFilesDropped: (files: File[]) => void | Promise<void>;
};

export type UseImageDropzoneReturn = {
  isDragOver: boolean;
  handlers: {
    onDragOver: (event: DragEvent<HTMLElement>) => void;
    onDragLeave: (event: DragEvent<HTMLElement>) => void;
    onDrop: (event: DragEvent<HTMLElement>) => void;
  };
  handlePaste: (event: ClipboardEvent<HTMLElement>) => void;
};

export const useImageDropzone = (
  options: UseImageDropzoneOptions
): UseImageDropzoneReturn => {
  const { enabled = true, onFilesDropped } = options;
  const [isDragOver, setIsDragOver] = useState(false);

  const filterImageFiles = useCallback((files: FileList | File[]) => {
    return Array.from(files).filter((file) =>
      SUPPORTED_IMAGE_MIME_TYPES.includes(file.type as SupportedImageMimeType)
    );
  }, []);

  const handleDragOver = useCallback(
    (event: DragEvent<HTMLElement>) => {
      event.preventDefault();
      event.stopPropagation();
      if (!enabled) return;
      setIsDragOver(true);
    },
    [enabled]
  );

  const handleDragLeave = useCallback((event: DragEvent<HTMLElement>) => {
    event.preventDefault();
    event.stopPropagation();
    setIsDragOver(false);
  }, []);

  const handleDrop = useCallback(
    async (event: DragEvent<HTMLElement>) => {
      event.preventDefault();
      event.stopPropagation();
      setIsDragOver(false);

      if (!enabled) return;

      const imageFiles = filterImageFiles(event.dataTransfer.files);
      if (imageFiles.length > 0) {
        await onFilesDropped(imageFiles);
      }
    },
    [enabled, onFilesDropped, filterImageFiles]
  );

  const handlePaste = useCallback(
    async (event: ClipboardEvent<HTMLElement>) => {
      if (!enabled) return;

      const items = event.clipboardData.items;
      const imageFiles: File[] = [];

      for (const item of Array.from(items)) {
        if (item.type.startsWith("image/")) {
          const file = item.getAsFile();
          if (file) imageFiles.push(file);
        }
      }

      if (imageFiles.length > 0) {
        event.preventDefault();
        await onFilesDropped(imageFiles);
      }
    },
    [enabled, onFilesDropped]
  );

  return {
    isDragOver,
    handlers: {
      onDragOver: handleDragOver,
      onDragLeave: handleDragLeave,
      onDrop: handleDrop,
    },
    handlePaste,
  };
};
