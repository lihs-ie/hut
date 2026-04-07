import { describe, it, expect, vi, beforeEach } from "vitest";

vi.mock("@/actions/auth", () => ({
  getSession: vi.fn(),
}));

class RedirectError extends Error {
  constructor(public readonly url: string) {
    super(`NEXT_REDIRECT: ${url}`);
  }
}

vi.mock("next/navigation", () => ({
  redirect: vi.fn((url: string) => {
    throw new RedirectError(url);
  }),
}));

vi.mock("@/config/routes", () => ({
  Routes: {
    admin: {
      articles: { list: "/admin/articles" },
      memos: { list: "/admin/memos" },
      series: { list: "/admin/series" },
      tag: { list: "/admin/tags" },
      profile: { edit: "/admin/profile" },
      privacy: { edit: "/admin/privacy" },
      analytics: { dashboard: "/admin/analytics" },
    },
  },
}));

vi.mock("../../src/app/admin/_components/organisms/admin/sidebar", () => ({
  AdminSidebar: () => null,
}));

vi.mock("@shared/components/atoms/icon/file-text", () => ({
  FileTextIcon: () => null,
}));

vi.mock("@shared/components/atoms/icon/message", () => ({
  MessageSquareIcon: () => null,
}));

vi.mock("@shared/components/atoms/icon/tag", () => ({
  TagIcon: () => null,
}));

vi.mock("@shared/components/atoms/icon/user", () => ({
  UserIcon: () => null,
}));

vi.mock("@shared/components/atoms/icon/shield", () => ({
  ShieldIcon: () => null,
}));

vi.mock("@shared/components/atoms/icon/bar-chart", () => ({
  BarChartIcon: () => null,
}));

vi.mock("../../src/app/admin/(dashboard)/layout.module.css", () => ({
  default: { container: "container", content: "content" },
}));

import { getSession } from "@/actions/auth";
import { redirect } from "next/navigation";
import DashboardLayout from "../../src/app/admin/(dashboard)/layout";

describe("dashboard layout - 認証チェック", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("セッションが null の場合 /admin/login にリダイレクトする", async () => {
    vi.mocked(getSession).mockResolvedValue(null);

    await expect(DashboardLayout({ children: null })).rejects.toThrow(
      RedirectError,
    );
    expect(redirect).toHaveBeenCalledWith("/admin/login");
  });

  it("セッションが存在する場合 children をレンダリングする", async () => {
    vi.mocked(getSession).mockResolvedValue({
      uid: "test-uid",
      email: "test@example.com",
      displayName: null,
      photoURL: null,
    });

    await DashboardLayout({ children: "test-children" });

    expect(redirect).not.toHaveBeenCalled();
  });
});
