import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { imageSchema } from "@shared/domains/common/image";
import { ImageMold } from "../../support/molds/domains/common/image";

describe("domains/common/image", () => {
  describe("imageSchema", () => {
    describe("有効なImageの検証", () => {
      it("Forgerで生成したImageは有効", () => {
        const image = Forger(ImageMold).forge();
        const result = imageSchema.safeParse(image);
        expect(result.success).toBe(true);
      });

      it("有効なURLは有効", () => {
        const result = imageSchema.safeParse("https://example.com/image.png");
        expect(result.success).toBe(true);
      });

      it("picsum.photosのURLは有効", () => {
        const result = imageSchema.safeParse(
          "https://picsum.photos/seed/123/600/400"
        );
        expect(result.success).toBe(true);
      });

      it("HTTPSのURLは有効", () => {
        const result = imageSchema.safeParse(
          "https://cdn.example.com/images/photo.jpg"
        );
        expect(result.success).toBe(true);
      });

      it("複数のImageを生成できる", () => {
        const images = Forger(ImageMold).forgeMulti(5);
        for (const image of images) {
          const result = imageSchema.safeParse(image);
          expect(result.success).toBe(true);
        }
      });
    });

    describe("無効なImageの検証", () => {
      it("空文字列は無効", () => {
        const result = imageSchema.safeParse("");
        expect(result.success).toBe(false);
      });

      it("無効なURLは無効", () => {
        const result = imageSchema.safeParse("not-a-url");
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

      it("数値は無効", () => {
        const result = imageSchema.safeParse(12345);
        expect(result.success).toBe(false);
      });

      it("オブジェクトは無効", () => {
        const result = imageSchema.safeParse({
          url: "https://example.com/image.png",
        });
        expect(result.success).toBe(false);
      });
    });
  });
});
