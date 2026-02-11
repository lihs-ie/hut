import { describe, it, expect } from "vitest";
import { Routes } from "@/config/routes";

describe("config/routes", () => {
  describe("admin.analytics", () => {
    it("analyticsルートが定義されている", () => {
      expect(Routes.admin.analytics).toBeDefined();
    });

    it("analytics.dashboardのパスが/admin/analyticsである", () => {
      expect(Routes.admin.analytics.dashboard).toBe("/admin/analytics");
    });
  });

  describe("既存ルートとの整合性", () => {
    it("admin.dashboardが存在する", () => {
      expect(Routes.admin.dashboard).toBe("/admin");
    });

    it("admin.articles.listが存在する", () => {
      expect(Routes.admin.articles.list).toBe("/admin/articles");
    });

    it("admin.memos.listが存在する", () => {
      expect(Routes.admin.memos.list).toBe("/admin/memos");
    });

    it("admin.tag.listが存在する", () => {
      expect(Routes.admin.tag.list).toBe("/admin/tags");
    });

    it("admin.profile.editが存在する", () => {
      expect(Routes.admin.profile.edit).toBe("/admin/profile/edit");
    });

    it("admin.privacy.editが存在する", () => {
      expect(Routes.admin.privacy.edit).toBe("/admin/privacy/edit");
    });
  });
});
