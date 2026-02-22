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

const DOWNLOAD_URL_PATTERN =
  /^https:\/\/firebasestorage\.googleapis\.com\/v0\/b\/test-bucket\/o\/admin%2Favatar\?alt=media&token=[0-9a-f-]+$/;

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
  });

  describe("Blob 入力でのアップロード", () => {
    it("アップロード成功時に download URL を返し、正しい contentType と file で保存する", async () => {
      const { FirebaseAdminStorageImageUploader } = await import(
        "@/infrastructure/storage"
      );
      const uploader = FirebaseAdminStorageImageUploader(mockBucket as never);
      const blob = createBlobLike("test-content", "image/png");

      const result = await uploader.upload(blob, "admin/avatar").unwrap();

      expect(result).toMatch(DOWNLOAD_URL_PATTERN);
      expect(mockBucket.file).toHaveBeenCalledWith("admin/avatar");
      expect(mockFile.save).toHaveBeenCalledWith(
        expect.any(Buffer),
        expect.objectContaining({
          contentType: "image/png",
          metadata: {
            firebaseStorageDownloadTokens: expect.stringMatching(
              /^[0-9a-f-]+$/,
            ),
          },
        }),
      );
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

      expect(result).toMatch(DOWNLOAD_URL_PATTERN);
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
  });
});
