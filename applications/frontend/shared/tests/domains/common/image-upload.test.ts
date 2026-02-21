import { describe, it, expect } from "vitest";
import {
  SUPPORTED_IMAGE_MIME_TYPES,
  MAX_IMAGE_SIZE_BEFORE_COMPRESSION,
  IMAGE_COMPRESSION_CONFIG,
  CONTENT_TYPES,
  imageUploadPathSchema,
  generateImageObjectName,
  generateUploadPath,
  validateImageFile,
  getExtensionFromMimeType,
} from "@shared/domains/common/image-upload";

describe("domains/common/image-upload", () => {
  describe("定数", () => {
    describe("SUPPORTED_IMAGE_MIME_TYPES", () => {
      it("サポートされるMIMEタイプが4つ存在する", () => {
        expect(SUPPORTED_IMAGE_MIME_TYPES).toHaveLength(4);
        expect(SUPPORTED_IMAGE_MIME_TYPES).toContain("image/jpeg");
        expect(SUPPORTED_IMAGE_MIME_TYPES).toContain("image/png");
        expect(SUPPORTED_IMAGE_MIME_TYPES).toContain("image/webp");
        expect(SUPPORTED_IMAGE_MIME_TYPES).toContain("image/gif");
      });
    });

    describe("MAX_IMAGE_SIZE_BEFORE_COMPRESSION", () => {
      it("10MBである", () => {
        expect(MAX_IMAGE_SIZE_BEFORE_COMPRESSION).toBe(10 * 1024 * 1024);
      });
    });

    describe("IMAGE_COMPRESSION_CONFIG", () => {
      it("設定値が正しい", () => {
        expect(IMAGE_COMPRESSION_CONFIG.maxWidth).toBe(1200);
        expect(IMAGE_COMPRESSION_CONFIG.maxHeight).toBe(1200);
        expect(IMAGE_COMPRESSION_CONFIG.quality).toBe(0.8);
      });
    });

    describe("CONTENT_TYPES", () => {
      it("3つのコンテンツタイプを持つ", () => {
        expect(CONTENT_TYPES).toHaveLength(3);
        expect(CONTENT_TYPES).toContain("article");
        expect(CONTENT_TYPES).toContain("memo");
        expect(CONTENT_TYPES).toContain("chapter");
      });
    });
  });

  describe("imageUploadPathSchema", () => {
    it("任意の文字列は有効", () => {
      const result = imageUploadPathSchema.safeParse("articles/123/image.png");
      expect(result.success).toBe(true);
    });

    it("空文字列も有効", () => {
      const result = imageUploadPathSchema.safeParse("");
      expect(result.success).toBe(true);
    });

    it("nullは無効", () => {
      const result = imageUploadPathSchema.safeParse(null);
      expect(result.success).toBe(false);
    });

    it("undefinedは無効", () => {
      const result = imageUploadPathSchema.safeParse(undefined);
      expect(result.success).toBe(false);
    });

    it("数値は無効", () => {
      const result = imageUploadPathSchema.safeParse(123);
      expect(result.success).toBe(false);
    });
  });

  describe("generateImageObjectName", () => {
    it("identifierと拡張子からオブジェクト名を生成する", () => {
      const objectName = generateImageObjectName("01ABCDEF", "webp");
      expect(objectName).toBe("01ABCDEF.webp");
    });

    it("gif拡張子でも正しく生成する", () => {
      const objectName = generateImageObjectName("01ABCDEF", "gif");
      expect(objectName).toBe("01ABCDEF.gif");
    });
  });

  describe("generateUploadPath", () => {
    it("articleの場合は正しいパスを生成する", () => {
      const path = generateUploadPath("article", "ref123", "image.webp");
      expect(path).toBe("articles/ref123/image.webp");
    });

    it("memoの場合は正しいパスを生成する", () => {
      const path = generateUploadPath("memo", "ref456", "photo.webp");
      expect(path).toBe("memos/ref456/photo.webp");
    });

    it("chapterの場合はエラーをスローする", () => {
      expect(() => {
        generateUploadPath("chapter", "ref789", "image.webp");
      }).toThrow("Chapter upload requires separate handling");
    });
  });

  describe("validateImageFile", () => {
    const createMockFile = (
      size: number,
      type: string,
      name: string = "test.png"
    ): File => {
      const blob = new Blob([new ArrayBuffer(size)], { type });
      return new File([blob], name, { type });
    };

    describe("有効なファイルの検証", () => {
      it.each([
        ["JPEG", "image/jpeg"],
        ["PNG", "image/png"],
        ["WebP", "image/webp"],
        ["GIF", "image/gif"],
      ])("%sファイルは有効", (_name, mimeType) => {
        const file = createMockFile(1024, mimeType);
        const result = validateImageFile(file);
        expect(result.isOk).toBe(true);
      });

      it("10MB未満のファイルは有効", () => {
        const file = createMockFile(
          MAX_IMAGE_SIZE_BEFORE_COMPRESSION - 1,
          "image/png"
        );
        const result = validateImageFile(file);
        expect(result.isOk).toBe(true);
      });
    });

    describe("無効なファイルの検証", () => {
      it("サポートされていないMIMEタイプは無効", () => {
        const file = createMockFile(1024, "image/bmp");
        const result = validateImageFile(file);
        expect(result.isErr).toBe(true);
        if (result.isErr) {
          expect(result.unwrapError().field).toBe("imageType");
        }
      });

      it("PDFは無効", () => {
        const file = createMockFile(1024, "application/pdf");
        const result = validateImageFile(file);
        expect(result.isErr).toBe(true);
      });

      it("10MBを超えるファイルは無効", () => {
        const file = createMockFile(
          MAX_IMAGE_SIZE_BEFORE_COMPRESSION + 1,
          "image/png"
        );
        const result = validateImageFile(file);
        expect(result.isErr).toBe(true);
        if (result.isErr) {
          expect(result.unwrapError().field).toBe("imageSize");
        }
      });
    });
  });

  describe("getExtensionFromMimeType", () => {
    it.each([
      ["image/jpeg", "jpg"],
      ["image/png", "png"],
      ["image/webp", "webp"],
      ["image/gif", "gif"],
      ["image/unknown", "jpg"],
      ["", "jpg"],
    ])("%sは%sを返す", (mimeType, expected) => {
      expect(getExtensionFromMimeType(mimeType)).toBe(expected);
    });
  });
});
