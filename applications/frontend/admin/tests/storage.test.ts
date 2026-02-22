import { describe, it, expect, vi, beforeEach } from "vitest";
import { isUnexpectedError } from "@shared/aspects/error";

type MockFile = {
  save: ReturnType<typeof vi.fn>;
};

type MockBucket = {
  file: ReturnType<typeof vi.fn>;
  name: string;
};

const createMockFile = (): MockFile => ({
  save: vi.fn().mockResolvedValue(undefined),
});

const createMockBucket = (mockFile: MockFile): MockBucket => ({
  file: vi.fn().mockReturnValue(mockFile),
  name: "test-bucket",
});

const mockGetDownloadURL = vi.fn();

vi.mock("firebase-admin/storage", () => ({
  getDownloadURL: (...args: unknown[]) => mockGetDownloadURL(...args),
}));

const DOWNLOAD_URL =
  "https://firebasestorage.googleapis.com/v0/b/test-bucket/o/admin%2Favatar?alt=media&token=test-token";

const createBlobLike = (
  content: string,
  type: string,
): { arrayBuffer: () => Promise<ArrayBuffer>; type: string } => {
  const buffer = Buffer.from(content);
  return {
    arrayBuffer: () =>
      Promise.resolve(
        buffer.buffer.slice(
          buffer.byteOffset,
          buffer.byteOffset + buffer.byteLength,
        ),
      ),
    type,
  };
};

describe("FirebaseAdminStorageImageUploader", () => {
  let mockFile: MockFile;
  let mockBucket: MockBucket;

  beforeEach(() => {
    vi.clearAllMocks();
    mockFile = createMockFile();
    mockBucket = createMockBucket(mockFile);
    mockGetDownloadURL.mockResolvedValue(DOWNLOAD_URL);
  });

  describe("Blob 入力でのアップロード", () => {
    it("アップロード成功時に download URL を返し、正しい contentType と file で保存する", async () => {
      const { FirebaseAdminStorageImageUploader } = await import(
        "@/infrastructure/storage"
      );
      const uploader = FirebaseAdminStorageImageUploader(mockBucket as never);
      const blob = createBlobLike("test-content", "image/png");

      const result = await uploader.upload(blob, "admin/avatar").unwrap();

      expect(result).toBe(DOWNLOAD_URL);
      expect(mockBucket.file).toHaveBeenCalledWith("admin/avatar");
      expect(mockFile.save).toHaveBeenCalledWith(
        expect.any(Buffer),
        expect.objectContaining({ contentType: "image/png" }),
      );
      expect(mockGetDownloadURL).toHaveBeenCalledWith(mockFile);
    });

    it("content type が空の場合 application/octet-stream を使用する", async () => {
      const { FirebaseAdminStorageImageUploader } = await import(
        "@/infrastructure/storage"
      );
      const uploader = FirebaseAdminStorageImageUploader(mockBucket as never);
      const blob = createBlobLike("binary-content", "");

      await uploader.upload(blob, "admin/avatar").unwrap();

      expect(mockFile.save).toHaveBeenCalledWith(
        expect.any(Buffer),
        expect.objectContaining({ contentType: "application/octet-stream" }),
      );
    });
  });

  describe("data URL 入力でのアップロード", () => {
    it("data URL をパースして正しい contentType とバッファでアップロードする", async () => {
      const { FirebaseAdminStorageImageUploader } = await import(
        "@/infrastructure/storage"
      );
      const uploader = FirebaseAdminStorageImageUploader(mockBucket as never);
      const base64Content = Buffer.from("test-image-data").toString("base64");
      const dataUrl = `data:image/webp;base64,${base64Content}`;

      const result = await uploader.upload(dataUrl, "admin/avatar").unwrap();

      expect(result).toBe(DOWNLOAD_URL);
      expect(mockFile.save).toHaveBeenCalledWith(
        Buffer.from("test-image-data"),
        expect.objectContaining({ contentType: "image/webp" }),
      );
    });
  });

  describe("エラーハンドリング", () => {
    it("save が失敗した場合 UnexpectedError を返す", async () => {
      mockFile.save.mockRejectedValue(new Error("Upload failed"));

      const { FirebaseAdminStorageImageUploader } = await import(
        "@/infrastructure/storage"
      );
      const uploader = FirebaseAdminStorageImageUploader(mockBucket as never);
      const blob = createBlobLike("content", "image/png");

      const error = await uploader
        .upload(blob, "admin/avatar")
        .unwrapError();

      expect(isUnexpectedError(error)).toBe(true);
    });

    it("getDownloadURL が失敗した場合 UnexpectedError を返す", async () => {
      mockGetDownloadURL.mockRejectedValue(
        new Error("Failed to get download URL"),
      );

      const { FirebaseAdminStorageImageUploader } = await import(
        "@/infrastructure/storage"
      );
      const uploader = FirebaseAdminStorageImageUploader(mockBucket as never);
      const blob = createBlobLike("content", "image/png");

      const error = await uploader
        .upload(blob, "admin/avatar")
        .unwrapError();

      expect(isUnexpectedError(error)).toBe(true);
    });
  });
});
