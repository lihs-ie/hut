import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  imageIdentifier,
  imageTypeSchema,
  ImageType,
  imageURLSchema,
  uploadStatusSchema,
  UploadStatus,
  imageSchema,
  validateImage,
  uploaded,
  criteriaSchema,
} from "@shared/domains/image";
import {
  ImageIdentifierMold,
  ImageTypeMold,
  ImageURLMold,
  UploadStatusMold,
  ImageMold,
  ImageCriteriaMold,
} from "../../support/molds/domains/image";
import { ArticleIdentifierMold } from "../../support/molds/domains/article";
import {
  describeIdentifierSchema,
  describeEnumSchema,
} from "../../support/helpers/schema-test";

describe("domains/image/common", () => {
  describe("imageIdentifier", () => {
    describeIdentifierSchema(
      "ImageIdentifier",
      imageIdentifier,
      () => Forger(ImageIdentifierMold).forge(),
      (count) => Forger(ImageIdentifierMold).forgeMulti(count)
    );
  });

  describe("imageTypeSchema", () => {
    describeEnumSchema(
      "ImageType",
      imageTypeSchema,
      ["png", "jpeg", "gif", "webp"],
      {
        PNG: ImageType.PNG,
        JPEG: ImageType.JPEG,
        GIF: ImageType.GIF,
        WEBP: ImageType.WEBP,
      }
    );

    it("Forgerで生成したタイプは有効", () => {
      const type = Forger(ImageTypeMold).forge();
      const result = imageTypeSchema.safeParse(type);
      expect(result.success).toBe(true);
    });
  });

  describe("imageURLSchema", () => {
    describe("有効なImageURLの検証", () => {
      it("Forgerで生成したURLは有効", () => {
        const url = Forger(ImageURLMold).forge();
        const result = imageURLSchema.safeParse(url);
        expect(result.success).toBe(true);
      });

      it("HTTPSのURLは有効", () => {
        const result = imageURLSchema.safeParse(
          "https://example.com/image.png"
        );
        expect(result.success).toBe(true);
      });
    });

    describe("無効なImageURLの検証", () => {
      it("不正なURLは無効", () => {
        const result = imageURLSchema.safeParse("not-a-url");
        expect(result.success).toBe(false);
      });

      it("空文字列は無効", () => {
        const result = imageURLSchema.safeParse("");
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = imageURLSchema.safeParse(null);
        expect(result.success).toBe(false);
      });
    });
  });

  describe("uploadStatusSchema", () => {
    describeEnumSchema(
      "UploadStatus",
      uploadStatusSchema,
      ["pending", "completed", "failed"],
      {
        PENDING: UploadStatus.PENDING,
        COMPLETED: UploadStatus.COMPLETED,
        FAILED: UploadStatus.FAILED,
      }
    );

    it("Forgerで生成したステータスは有効", () => {
      const status = Forger(UploadStatusMold).forge();
      const result = uploadStatusSchema.safeParse(status);
      expect(result.success).toBe(true);
    });
  });

  describe("imageSchema", () => {
    describe("有効なImageの検証", () => {
      it("uploadStatusがcompletedでurlがある場合は有効", () => {
        const image = Forger(ImageMold).forge({
          uploadStatus: UploadStatus.COMPLETED,
          url: Forger(ImageURLMold).forge(),
        });
        const result = imageSchema.safeParse(image);
        expect(result.success).toBe(true);
      });

      it("uploadStatusがpendingでurlがnullの場合は有効", () => {
        const image = Forger(ImageMold).forge({
          uploadStatus: UploadStatus.PENDING,
          url: null,
        });
        const result = imageSchema.safeParse(image);
        expect(result.success).toBe(true);
      });

      it("uploadStatusがfailedでurlがnullの場合は有効", () => {
        const image = Forger(ImageMold).forge({
          uploadStatus: UploadStatus.FAILED,
          url: null,
        });
        const result = imageSchema.safeParse(image);
        expect(result.success).toBe(true);
      });
    });

    describe("無効なImageの検証", () => {
      it("uploadStatusがcompletedでurlがnullの場合は無効", () => {
        const result = imageSchema.safeParse({
          identifier: Forger(ImageIdentifierMold).forge(),
          type: ImageType.PNG,
          url: null,
          uploadStatus: UploadStatus.COMPLETED,
          reference: Forger(ArticleIdentifierMold).forge(),
          content: "article",
        });
        expect(result.success).toBe(false);
      });

      it("uploadStatusがpendingでurlがある場合は無効", () => {
        const result = imageSchema.safeParse({
          identifier: Forger(ImageIdentifierMold).forge(),
          type: ImageType.PNG,
          url: "https://example.com/image.png",
          uploadStatus: UploadStatus.PENDING,
          reference: Forger(ArticleIdentifierMold).forge(),
          content: "article",
        });
        expect(result.success).toBe(false);
      });

      it("identifierが無効な場合は無効", () => {
        const result = imageSchema.safeParse({
          identifier: "invalid",
          type: ImageType.PNG,
          url: null,
          uploadStatus: UploadStatus.PENDING,
          reference: Forger(ArticleIdentifierMold).forge(),
          content: "article",
        });
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = imageSchema.safeParse(null);
        expect(result.success).toBe(false);
      });

      it("undefinedは無効", () => {
        const result = imageSchema.safeParse(undefined);
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateImage", () => {
    it("referenceが欠けている場合はerrを返す", () => {
      const result = validateImage({
        identifier: Forger(ImageIdentifierMold).forge(),
        type: "png",
        url: null,
        uploadStatus: "pending",
        content: "article",
      });
      expect(result.isErr).toBe(true);
    });

    it("無効なUnvalidatedImageでerrを返す", () => {
      const result = validateImage({
        identifier: "invalid",
        type: "invalid",
        url: null,
        uploadStatus: "invalid",
        content: "article",
      });
      expect(result.isErr).toBe(true);
    });
  });

  describe("uploaded", () => {
    it("画像のURLを設定し、ステータスをCOMPLETEDに変更する", () => {
      const originalImage = Forger(ImageMold).forge({
        uploadStatus: UploadStatus.PENDING,
        url: null,
      });
      const newUrl = imageURLSchema.parse("https://example.com/uploaded.png");
      const uploadedImage = uploaded(originalImage, newUrl);

      expect(uploadedImage.url).toBe(newUrl);
      expect(uploadedImage.uploadStatus).toBe(UploadStatus.COMPLETED);
      expect(uploadedImage.identifier).toBe(originalImage.identifier);
      expect(uploadedImage.type).toBe(originalImage.type);
    });
  });

  describe("criteriaSchema", () => {
    describe("有効なCriteriaの検証", () => {
      it("Forgerで生成したCriteriaは有効", () => {
        const criteria = Forger(ImageCriteriaMold).forge();
        const result = criteriaSchema.safeParse(criteria);
        expect(result.success).toBe(true);
      });

      it("全てのフィールドがundefinedでも有効", () => {
        const result = criteriaSchema.safeParse({});
        expect(result.success).toBe(true);
      });

      it("typeのみ指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          type: ImageType.PNG,
        });
        expect(result.success).toBe(true);
      });

      it("uploadStatusのみ指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          uploadStatus: UploadStatus.COMPLETED,
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なCriteriaの検証", () => {
      it("typeが無効な場合は無効", () => {
        const result = criteriaSchema.safeParse({
          type: "invalid",
        });
        expect(result.success).toBe(false);
      });

      it("uploadStatusが無効な場合は無効", () => {
        const result = criteriaSchema.safeParse({
          uploadStatus: "invalid",
        });
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = criteriaSchema.safeParse(null);
        expect(result.success).toBe(false);
      });
    });
  });
});
