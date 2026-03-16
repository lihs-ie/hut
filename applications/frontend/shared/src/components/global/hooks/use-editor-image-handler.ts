"use client";

import { useCallback, useRef } from "react";
import {
  SUPPORTED_IMAGE_MIME_TYPES,
  SupportedImageMimeType,
} from "@shared/domains/common/image-upload";

type Return = {
  handlePaste: (event: ClipboardEvent) => boolean;
  handleDrop: (files: File[]) => void;
  setProcessFiles: (processFiles: (files: File[]) => Promise<void>) => void;
};

export const useEditorImageHandler = (): Return => {
  const processFilesRef = useRef<((files: File[]) => Promise<void>) | null>(
    null
  );

  const setProcessFiles = useCallback(
    (processFiles: (files: File[]) => Promise<void>) => {
      processFilesRef.current = processFiles;
    },
    []
  );

  const handlePaste = useCallback((event: ClipboardEvent): boolean => {
    const items = event.clipboardData?.items;
    if (!items) return false;
    const imageFiles: File[] = [];
    for (const item of Array.from(items)) {
      if (
        SUPPORTED_IMAGE_MIME_TYPES.includes(
          item.type as SupportedImageMimeType
        )
      ) {
        const file = item.getAsFile();
        if (file) imageFiles.push(file);
      }
    }
    if (imageFiles.length === 0) return false;
    event.preventDefault();
    processFilesRef.current?.(imageFiles);
    return true;
  }, []);

  const handleDrop = useCallback((files: File[]) => {
    processFilesRef.current?.(files);
  }, []);

  return { handlePaste, handleDrop, setProcessFiles };
};
