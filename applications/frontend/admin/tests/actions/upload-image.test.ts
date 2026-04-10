/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

vi.mock("@/actions/auth", () => ({
  isAdmin: vi.fn(),
}));

vi.mock("@/providers/infrastructure/storage", () => ({
  AdminImageUploaderProvider: {
    firebaseAdmin: {
      upload: vi.fn(),
    },
  },
}));

describe("uploadImage - パストラバーサル防止", () => {
  beforeEach(async () => {
    vi.clearAllMocks();
    vi.resetModules();
  });

  describe("認証チェック", () => {
    it("未認証の場合はエラーを throw する", async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(false);

      const { uploadImage } = await import("@/actions/common");

      await expect(
        uploadImage(new Blob(["test"]), "images/articles/test.png"),
      ).rejects.toThrow();
    });
  });

  describe("パストラバーサル防止", () => {
    beforeEach(async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(true);
    });

    const traversalPaths = [
      "../secret/file.png",
      "images/../../../etc/passwd",
      "images/articles/../../secret.png",
      "./images/articles/test.png",
      "images/articles/./../../secret.png",
    ];

    traversalPaths.forEach((path) => {
      it(`パストラバーサルパス "${path}" を拒否する`, async () => {
        const { uploadImage } = await import("@/actions/common");

        await expect(
          uploadImage(new Blob(["test"]), path),
        ).rejects.toThrow();
      });
    });
  });

  describe("許可されたプレフィックスのみ受け入れる", () => {
    beforeEach(async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(true);
    });

    const disallowedPaths = [
      "secret/file.png",
      "admin/config.png",
      "system/file.png",
      "users/profile.png",
    ];

    disallowedPaths.forEach((path) => {
      it(`許可されていないプレフィックス "${path}" を拒否する`, async () => {
        const { uploadImage } = await import("@/actions/common");

        await expect(
          uploadImage(new Blob(["test"]), path),
        ).rejects.toThrow();
      });
    });
  });

  describe("Content-Typeバリデーション", () => {
    beforeEach(async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(true);
    });

    it("Content-Typeが空のファイルを拒否する", async () => {
      const { uploadImage } = await import("@/actions/common");
      const emptyTypeFile = new Blob(["content"]);

      await expect(
        uploadImage(emptyTypeFile, "images/articles/test.png"),
      ).rejects.toThrow();
    });

    it("非画像ファイル (text/plain) を拒否する", async () => {
      const { uploadImage } = await import("@/actions/common");
      const textFile = new Blob(["malicious content"], { type: "text/plain" });

      await expect(
        uploadImage(textFile, "images/articles/test.txt"),
      ).rejects.toThrow();
    });

    it("非画像ファイル (application/javascript) を拒否する", async () => {
      const { uploadImage } = await import("@/actions/common");
      const scriptFile = new Blob(["alert(1)"], {
        type: "application/javascript",
      });

      await expect(
        uploadImage(scriptFile, "images/articles/test.js"),
      ).rejects.toThrow();
    });

    it("非画像ファイル (application/x-php) を拒否する", async () => {
      const { uploadImage } = await import("@/actions/common");
      const phpFile = new Blob(["<?php system($_GET['cmd']); ?>"], {
        type: "application/x-php",
      });

      await expect(
        uploadImage(phpFile, "images/articles/test.php"),
      ).rejects.toThrow();
    });
  });

  describe("images/series/ プレフィックスも許可される", () => {
    beforeEach(async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(true);

      const { AdminImageUploaderProvider } = await import(
        "@/providers/infrastructure/storage"
      );
      vi.mocked(AdminImageUploaderProvider.firebaseAdmin.upload).mockReturnValue(
        {
          match: ({ ok }: { ok: (value: string) => string }) =>
            Promise.resolve(ok("https://example.com/series-image.png")),
        } as never,
      );
    });

    it("images/series/ プレフィックスのパスは通過する", async () => {
      const { uploadImage } = await import("@/actions/common");
      const imageFile = new Blob(["image data"], { type: "image/png" });

      await expect(
        uploadImage(imageFile, "images/series/ref012/cover.png"),
      ).resolves.toBe("https://example.com/series-image.png");
    });
  });

  describe("images/chapters/ プレフィックスも許可される", () => {
    beforeEach(async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(true);

      const { AdminImageUploaderProvider } = await import(
        "@/providers/infrastructure/storage"
      );
      vi.mocked(AdminImageUploaderProvider.firebaseAdmin.upload).mockReturnValue(
        {
          match: ({ ok }: { ok: (value: string) => string }) =>
            Promise.resolve(ok("https://example.com/chapter-image.png")),
        } as never,
      );
    });

    it("images/chapters/ プレフィックスのパスは通過する", async () => {
      const { uploadImage } = await import("@/actions/common");
      const imageFile = new Blob(["image data"], { type: "image/png" });

      await expect(
        uploadImage(imageFile, "images/chapters/ref123/image.png"),
      ).resolves.toBe("https://example.com/chapter-image.png");
    });
  });

  describe("許可された画像Content-Typeは通過する", () => {
    beforeEach(async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(true);

      const { AdminImageUploaderProvider } = await import(
        "@/providers/infrastructure/storage"
      );
      vi.mocked(AdminImageUploaderProvider.firebaseAdmin.upload).mockReturnValue(
        {
          match: ({ ok }: { ok: (value: string) => string }) =>
            Promise.resolve(ok("https://example.com/image.png")),
        } as never,
      );
    });

    const allowedImageTypes = [
      { type: "image/jpeg", path: "images/articles/test.jpg" },
      { type: "image/png", path: "images/articles/test.png" },
      { type: "image/webp", path: "images/articles/test.webp" },
      { type: "image/gif", path: "images/articles/test.gif" },
    ];

    allowedImageTypes.forEach(({ type, path }) => {
      it(`${type} は通過する`, async () => {
        const { uploadImage } = await import("@/actions/common");
        const imageFile = new Blob(["image data"], { type });

        await expect(uploadImage(imageFile, path)).resolves.toBe(
          "https://example.com/image.png",
        );
      });
    });
  });
});
