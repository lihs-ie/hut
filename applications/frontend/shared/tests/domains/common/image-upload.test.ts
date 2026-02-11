import { describe, it, expect } from "vitest";
import {
  SUPPORTED_IMAGE_MIME_TYPES,
  MAX_IMAGE_SIZE_BEFORE_COMPRESSION,
  IMAGE_COMPRESSION_CONFIG,
  CONTENT_TYPES,
  imageUploadPathSchema,
  generateImageFileName,
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

  describe("generateImageFileName", () => {
    it("タイムスタンプ付きのファイル名を生成する", () => {
      const fileName = generateImageFileName("test.png", "png");
      expect(fileName).toMatch(/^\d+_test\.png$/);
    });

    it("拡張子を変更できる", () => {
      const fileName = generateImageFileName("image.png", "jpg");
      expect(fileName).toMatch(/^\d+_image\.jpg$/);
    });

    it("特殊文字をアンダースコアに置換する", () => {
      const fileName = generateImageFileName("test@file#name.png", "png");
      expect(fileName).toMatch(/^\d+_test_file_name\.png$/);
    });

    it("元の拡張子を除去する", () => {
      const fileName = generateImageFileName("image.png", "webp");
      expect(fileName).toMatch(/^\d+_image\.webp$/);
      expect(fileName).not.toContain(".png");
    });

    it("長いファイル名を50文字に切り詰める", () => {
      const longName = "a".repeat(100) + ".png";
      const fileName = generateImageFileName(longName, "png");
      const nameWithoutTimestampAndExtension = fileName
        .replace(/^\d+_/, "")
        .replace(/\.png$/, "");
      expect(nameWithoutTimestampAndExtension).toHaveLength(50);
    });

    it("日本語を含むファイル名をサニタイズする", () => {
      const fileName = generateImageFileName("テスト画像.png", "png");
      expect(fileName).toMatch(/^\d+_[_]*\.png$/);
    });
  });

  describe("generateUploadPath", () => {
    it("articleの場合は正しいパスを生成する", () => {
      const path = generateUploadPath("article", { primary: "123" }, "image.png");
      expect(path).toBe("articles/123/image.png");
    });

    it("memoの場合は正しいパスを生成する", () => {
      const path = generateUploadPath("memo", { primary: "456" }, "photo.jpg");
      expect(path).toBe("memos/456/photo.jpg");
    });

    it("chapterの場合はsecondary識別子が必要", () => {
      const path = generateUploadPath(
        "chapter",
        { primary: "series-1", secondary: "chapter-1" },
        "diagram.webp"
      );
      expect(path).toBe("series/series-1/chapters/chapter-1/diagram.webp");
    });

    it("chapterでsecondaryがない場合はエラーをスローする", () => {
      expect(() => {
        generateUploadPath("chapter", { primary: "series-1" }, "image.png");
      }).toThrow("Chapter requires secondary identifier (chapterId)");
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
