/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { connection } from "next/server";
import Page from "../../../src/app/privacy/page";
import { PrivacyIndex } from "@shared/components/templates/legal/privacy";
import { getPrivacyPolicy } from "@/actions/document";

vi.mock("next/server", () => ({ connection: vi.fn().mockResolvedValue(undefined) }));
vi.mock("@/actions/document", () => ({ getPrivacyPolicy: vi.fn() }));
vi.mock("@shared/components/templates/legal/privacy", () => ({ PrivacyIndex: vi.fn() }));

describe("/privacy page", () => {
  it("Reader のプライバシーポリシーをリクエスト時に取得する", async () => {
    const element = await Page();
    expect(connection).toHaveBeenCalled();
    expect(element.type).toBe(PrivacyIndex);
    expect(element.props.getPrivacy).toBe(getPrivacyPolicy);
  });
});
