/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { renderHook, act } from "@testing-library/react";
import { useImageDropzone } from "@shared/hooks/useImageDropzone";

const createMockDragEvent = (files: File[]) => {
  const dataTransfer = {
    files: files as unknown as FileList,
  };

  return {
    preventDefault: vi.fn(),
    stopPropagation: vi.fn(),
    dataTransfer,
  };
};

const createMockClipboardEvent = (files: File[]) => {
  const items = files.map((file) => ({
    type: file.type,
    getAsFile: () => file,
  }));

  return {
    preventDefault: vi.fn(),
    clipboardData: {
      items: items as unknown as DataTransferItemList,
    },
  };
};

describe("hooks/useImageDropzone", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe("isDragOver", () => {
    it("初期状態はfalse", () => {
      const onFilesDropped = vi.fn();
      const { result } = renderHook(() =>
        useImageDropzone({ onFilesDropped })
      );

      expect(result.current.isDragOver).toBe(false);
    });

    it("onDragOverでtrueになる", () => {
      const onFilesDropped = vi.fn();
      const { result } = renderHook(() =>
        useImageDropzone({ onFilesDropped })
      );

      const mockEvent = createMockDragEvent([]);

      act(() => {
        result.current.handlers.onDragOver(
          mockEvent as unknown as React.DragEvent<HTMLElement>
        );
      });

      expect(result.current.isDragOver).toBe(true);
      expect(mockEvent.preventDefault).toHaveBeenCalled();
      expect(mockEvent.stopPropagation).toHaveBeenCalled();
    });

    it("onDragLeaveでfalseになる", () => {
      const onFilesDropped = vi.fn();
      const { result } = renderHook(() =>
        useImageDropzone({ onFilesDropped })
      );

      const mockDragOverEvent = createMockDragEvent([]);
      const mockDragLeaveEvent = createMockDragEvent([]);

      act(() => {
        result.current.handlers.onDragOver(
          mockDragOverEvent as unknown as React.DragEvent<HTMLElement>
        );
      });

      expect(result.current.isDragOver).toBe(true);

      act(() => {
        result.current.handlers.onDragLeave(
          mockDragLeaveEvent as unknown as React.DragEvent<HTMLElement>
        );
      });

      expect(result.current.isDragOver).toBe(false);
    });

    it("onDropでfalseになる", async () => {
      const onFilesDropped = vi.fn();
      const { result } = renderHook(() =>
        useImageDropzone({ onFilesDropped })
      );

      const mockDragOverEvent = createMockDragEvent([]);
      const mockDropEvent = createMockDragEvent([]);

      act(() => {
        result.current.handlers.onDragOver(
          mockDragOverEvent as unknown as React.DragEvent<HTMLElement>
        );
      });

      expect(result.current.isDragOver).toBe(true);

      await act(async () => {
        await result.current.handlers.onDrop(
          mockDropEvent as unknown as React.DragEvent<HTMLElement>
        );
      });

      expect(result.current.isDragOver).toBe(false);
    });
  });

  describe("enabled", () => {
    it("enabled=falseの時、onDragOverでisDragOverが変化しない", () => {
      const onFilesDropped = vi.fn();
      const { result } = renderHook(() =>
        useImageDropzone({ enabled: false, onFilesDropped })
      );

      const mockEvent = createMockDragEvent([]);

      act(() => {
        result.current.handlers.onDragOver(
          mockEvent as unknown as React.DragEvent<HTMLElement>
        );
      });

      expect(result.current.isDragOver).toBe(false);
    });

    it("enabled=falseの時、onDropでonFilesDroppedが呼ばれない", async () => {
      const onFilesDropped = vi.fn();
      const { result } = renderHook(() =>
        useImageDropzone({ enabled: false, onFilesDropped })
      );

      const imageFile = new File(["test"], "test.png", { type: "image/png" });
      const mockEvent = createMockDragEvent([imageFile]);

      await act(async () => {
        await result.current.handlers.onDrop(
          mockEvent as unknown as React.DragEvent<HTMLElement>
        );
      });

      expect(onFilesDropped).not.toHaveBeenCalled();
    });

    it("enabled=falseの時、handlePasteでonFilesDroppedが呼ばれない", async () => {
      const onFilesDropped = vi.fn();
      const { result } = renderHook(() =>
        useImageDropzone({ enabled: false, onFilesDropped })
      );

      const imageFile = new File(["test"], "test.png", { type: "image/png" });
      const mockEvent = createMockClipboardEvent([imageFile]);

      await act(async () => {
        await result.current.handlePaste(
          mockEvent as unknown as React.ClipboardEvent<HTMLElement>
        );
      });

      expect(onFilesDropped).not.toHaveBeenCalled();
    });
  });

  describe("画像ファイルのフィルタリング", () => {
    it("画像ファイルのみがonFilesDroppedに渡される（onDrop）", async () => {
      const onFilesDropped = vi.fn();
      const { result } = renderHook(() =>
        useImageDropzone({ onFilesDropped })
      );

      const imageFile = new File(["test"], "test.png", { type: "image/png" });
      const textFile = new File(["test"], "test.txt", { type: "text/plain" });
      const mockEvent = createMockDragEvent([imageFile, textFile]);

      await act(async () => {
        await result.current.handlers.onDrop(
          mockEvent as unknown as React.DragEvent<HTMLElement>
        );
      });

      expect(onFilesDropped).toHaveBeenCalledWith([imageFile]);
    });

    it("画像ファイルのみがonFilesDroppedに渡される（handlePaste）", async () => {
      const onFilesDropped = vi.fn();
      const { result } = renderHook(() =>
        useImageDropzone({ onFilesDropped })
      );

      const imageFile = new File(["test"], "test.jpg", { type: "image/jpeg" });
      const mockEvent = createMockClipboardEvent([imageFile]);

      await act(async () => {
        await result.current.handlePaste(
          mockEvent as unknown as React.ClipboardEvent<HTMLElement>
        );
      });

      expect(onFilesDropped).toHaveBeenCalledWith([imageFile]);
      expect(mockEvent.preventDefault).toHaveBeenCalled();
    });

    it("画像ファイルがない場合はonFilesDroppedが呼ばれない", async () => {
      const onFilesDropped = vi.fn();
      const { result } = renderHook(() =>
        useImageDropzone({ onFilesDropped })
      );

      const textFile = new File(["test"], "test.txt", { type: "text/plain" });
      const mockEvent = createMockDragEvent([textFile]);

      await act(async () => {
        await result.current.handlers.onDrop(
          mockEvent as unknown as React.DragEvent<HTMLElement>
        );
      });

      expect(onFilesDropped).not.toHaveBeenCalled();
    });
  });

  describe("サポートされる画像形式", () => {
    const supportedTypes = [
      { type: "image/jpeg", extension: "jpg" },
      { type: "image/png", extension: "png" },
      { type: "image/gif", extension: "gif" },
      { type: "image/webp", extension: "webp" },
    ];

    supportedTypes.forEach(({ type, extension }) => {
      it(`${type}がサポートされる`, async () => {
        const onFilesDropped = vi.fn();
        const { result } = renderHook(() =>
          useImageDropzone({ onFilesDropped })
        );

        const imageFile = new File(["test"], `test.${extension}`, { type });
        const mockEvent = createMockDragEvent([imageFile]);

        await act(async () => {
          await result.current.handlers.onDrop(
            mockEvent as unknown as React.DragEvent<HTMLElement>
          );
        });

        expect(onFilesDropped).toHaveBeenCalledWith([imageFile]);
      });
    });
  });
});
