/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { renderHook, act, waitFor } from "@testing-library/react";
import { useImageUpload } from "@shared/hooks/useImageUpload";

vi.mock("@shared/hooks/useImageCompression", () => ({
  compressImageToWebP: vi.fn().mockImplementation((file: File) => ({
    _tag: "AsyncResult",
    match: vi
      .fn()
      .mockImplementation(
        async <U>(handlers: {
          ok: (value: unknown) => U;
          err: (error: unknown) => U;
        }) =>
          handlers.ok({
            blob: file,
            originalSize: file.size,
            compressedSize: file.size,
            width: 100,
            height: 100,
            mimeType: file.type === "image/gif" ? "image/gif" : "image/webp",
          }),
      ),
    unwrap: vi.fn().mockResolvedValue({
      blob: file,
      originalSize: file.size,
      compressedSize: file.size,
      width: 100,
      height: 100,
      mimeType: file.type === "image/gif" ? "image/gif" : "image/webp",
    }),
  })),
  getCompressedFileName: vi
    .fn()
    .mockImplementation((name: string, mimeType: string) =>
      mimeType === "image/gif" ? name : name.replace(/\.[^.]+$/, ".webp"),
    ),
  CompressedImage: {},
}));

describe("hooks/useImageUpload", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe("初期状態", () => {
    it("初期状態が正しく設定される", () => {
      const mockUploadAction = vi.fn();
      const { result } = renderHook(() =>
        useImageUpload({ uploadAction: mockUploadAction }),
      );

      expect(result.current.uploads).toEqual([]);
      expect(result.current.isUploading).toBe(false);
      expect(result.current.error).toBeNull();
      expect(result.current.isError).toBe(false);
    });
  });

  describe("uploadImage", () => {
    it("アップロードが成功する", async () => {
      const mockUploadAction = vi
        .fn()
        .mockResolvedValue("https://example.com/image.webp");
      const { result } = renderHook(() =>
        useImageUpload({ uploadAction: mockUploadAction }),
      );

      const file = new File(["test"], "test.jpg", { type: "image/jpeg" });

      await act(async () => {
        const uploadResult = await result.current.uploadImage(
          file,
          "article",
          "test-slug",
        );

        expect(uploadResult.isOk).toBe(true);
        if (uploadResult.isOk) {
          const value = uploadResult.unwrap();
          expect(value.url).toBe("https://example.com/image.webp");
          expect(value.placeholder.altText).toBe("test.jpg");
        }
      });

      expect(result.current.uploads.length).toBe(1);
      expect(result.current.uploads[0].status).toBe("completed");
    });

    it("アップロード中はisUploadingがtrueになる", async () => {
      let resolveUpload: (value: string) => void;
      const mockUploadAction = vi.fn(
        () =>
          new Promise<string>((resolve) => {
            resolveUpload = resolve;
          }),
      );

      const { result } = renderHook(() =>
        useImageUpload({ uploadAction: mockUploadAction }),
      );

      const file = new File(["test"], "test.jpg", { type: "image/jpeg" });

      act(() => {
        result.current.uploadImage(file, "article", "test-slug");
      });

      await waitFor(() => {
        expect(result.current.isUploading).toBe(true);
      });

      await act(async () => {
        resolveUpload!("https://example.com/image.webp");
      });

      await waitFor(() => {
        expect(result.current.isUploading).toBe(false);
      });
    });

    it("アップロード失敗時はfailed状態になる", async () => {
      const mockUploadAction = vi
        .fn()
        .mockRejectedValue(new Error("Upload failed"));
      const { result } = renderHook(() =>
        useImageUpload({ uploadAction: mockUploadAction }),
      );

      const file = new File(["test"], "test.jpg", { type: "image/jpeg" });

      await act(async () => {
        const uploadResult = await result.current.uploadImage(
          file,
          "article",
          "test-slug",
        );

        expect(uploadResult.isErr).toBe(true);
      });

      expect(result.current.uploads[0].status).toBe("failed");
      expect(result.current.uploads[0].error).toBe("Upload failed");
      expect(result.current.isError).toBe(true);
      expect(result.current.error?.message).toBe("Upload failed");
    });

    it("アップロード失敗後にclearErrorでエラーをクリアできる", async () => {
      const mockUploadAction = vi
        .fn()
        .mockRejectedValue(new Error("Upload failed"));
      const { result } = renderHook(() =>
        useImageUpload({ uploadAction: mockUploadAction }),
      );

      const file = new File(["test"], "test.jpg", { type: "image/jpeg" });

      await act(async () => {
        await result.current.uploadImage(file, "article", "test-slug");
      });

      expect(result.current.isError).toBe(true);

      act(() => {
        result.current.clearError();
      });

      expect(result.current.isError).toBe(false);
      expect(result.current.error).toBeNull();
    });

    it("プレースホルダー情報が正しく生成される", async () => {
      const mockUploadAction = vi
        .fn()
        .mockResolvedValue("https://example.com/image.webp");
      const { result } = renderHook(() =>
        useImageUpload({ uploadAction: mockUploadAction }),
      );

      const file = new File(["test"], "screenshot.png", { type: "image/png" });

      await act(async () => {
        const uploadResult = await result.current.uploadImage(
          file,
          "article",
          "test-slug",
        );

        expect(uploadResult.isOk).toBe(true);
        if (uploadResult.isOk) {
          const value = uploadResult.unwrap();
          expect(value.placeholder.altText).toBe("screenshot.png");
          expect(value.placeholder.placeholder).toMatch(
            /!\[uploading\.\.\.\]\(placeholder-[A-Z0-9]+\)/,
          );
        }
      });
    });
  });

  describe("cancelUpload", () => {
    it("キャンセル時はfailed状態になる", async () => {
      let resolveUpload: (value: string) => void;
      const mockUploadAction = vi.fn(
        () =>
          new Promise<string>((resolve) => {
            resolveUpload = resolve;
          }),
      );

      const { result } = renderHook(() =>
        useImageUpload({ uploadAction: mockUploadAction }),
      );

      const file = new File(["test"], "test.jpg", { type: "image/jpeg" });

      act(() => {
        result.current.uploadImage(file, "article", "test-slug");
      });

      await waitFor(() => {
        expect(result.current.uploads.length).toBe(1);
      });

      const uploadId = result.current.uploads[0].id;

      act(() => {
        result.current.cancelUpload(uploadId);
      });

      expect(result.current.uploads[0].status).toBe("failed");
      expect(result.current.uploads[0].error).toBe("キャンセルされました");
    });
  });

  describe("clearCompleted", () => {
    it("完了済みとエラーのアップロードをクリアする", async () => {
      const mockUploadAction = vi
        .fn()
        .mockResolvedValueOnce("https://example.com/image1.webp")
        .mockRejectedValueOnce(new Error("Failed"));

      const { result } = renderHook(() =>
        useImageUpload({ uploadAction: mockUploadAction }),
      );

      const file1 = new File(["test1"], "test1.jpg", { type: "image/jpeg" });
      const file2 = new File(["test2"], "test2.jpg", { type: "image/jpeg" });

      await act(async () => {
        await result.current.uploadImage(file1, "article", "test-slug");
      });

      await act(async () => {
        await result.current.uploadImage(file2, "article", "test-slug");
      });

      expect(result.current.uploads.length).toBe(2);
      expect(result.current.uploads[0].status).toBe("completed");
      expect(result.current.uploads[1].status).toBe("failed");

      act(() => {
        result.current.clearCompleted();
      });

      expect(result.current.uploads.length).toBe(0);
    });
  });

  describe("複数ファイルのアップロード", () => {
    it("複数ファイルを順次アップロードできる", async () => {
      const mockUploadAction = vi
        .fn()
        .mockResolvedValueOnce("https://example.com/image1.webp")
        .mockResolvedValueOnce("https://example.com/image2.webp");

      const { result } = renderHook(() =>
        useImageUpload({ uploadAction: mockUploadAction }),
      );

      const file1 = new File(["test1"], "test1.jpg", { type: "image/jpeg" });
      const file2 = new File(["test2"], "test2.png", { type: "image/png" });

      await act(async () => {
        await result.current.uploadImage(file1, "article", "test-slug");
        await result.current.uploadImage(file2, "article", "test-slug");
      });

      expect(result.current.uploads.length).toBe(2);
      expect(result.current.uploads[0].status).toBe("completed");
      expect(result.current.uploads[1].status).toBe("completed");
      expect(mockUploadAction).toHaveBeenCalledTimes(2);
    });
  });

  describe("contentType", () => {
    it("articleタイプでアップロードパスが生成される", async () => {
      const mockUploadAction = vi
        .fn()
        .mockResolvedValue("https://example.com/image.webp");
      const { result } = renderHook(() =>
        useImageUpload({ uploadAction: mockUploadAction }),
      );

      const file = new File(["test"], "test.jpg", { type: "image/jpeg" });

      await act(async () => {
        await result.current.uploadImage(file, "article", "my-article");
      });

      expect(mockUploadAction).toHaveBeenCalledWith(
        expect.any(File),
        expect.stringContaining("articles/my-article/"),
      );
    });

    it("memoタイプでアップロードパスが生成される", async () => {
      const mockUploadAction = vi
        .fn()
        .mockResolvedValue("https://example.com/image.webp");
      const { result } = renderHook(() =>
        useImageUpload({ uploadAction: mockUploadAction }),
      );

      const file = new File(["test"], "test.jpg", { type: "image/jpeg" });

      await act(async () => {
        await result.current.uploadImage(file, "memo", "my-memo");
      });

      expect(mockUploadAction).toHaveBeenCalledWith(
        expect.any(File),
        expect.stringContaining("memos/my-memo/"),
      );
    });
  });
});
