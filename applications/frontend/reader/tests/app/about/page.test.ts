/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { connection } from "next/server";
import Page from "../../../src/app/about/page";
import { AboutIndex } from "@shared/components/templates/about";
import { getProfile } from "@/actions/admin";
import { getAllTags } from "@/actions/tag";

vi.mock("next/server", () => ({ connection: vi.fn().mockResolvedValue(undefined) }));
vi.mock("@/actions/admin", () => ({ getProfile: vi.fn() }));
vi.mock("@/actions/tag", () => ({ getAllTags: vi.fn() }));
vi.mock("@shared/components/templates/about", () => ({ AboutIndex: vi.fn() }));

describe("/about page", () => {
  it("Reader のプロフィールをリクエスト時に取得する", async () => {
    const element = await Page();
    expect(connection).toHaveBeenCalled();
    expect(element.type).toBe(AboutIndex);
    expect(element.props.getProfile).toBe(getProfile);
    expect(element.props.getAllTags).toBe(getAllTags);
  });
});
