/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";

vi.mock("next/link", () => ({
  default: (props: { href: string; children: React.ReactNode; className?: string }) => (
    <a href={props.href} className={props.className}>{props.children}</a>
  ),
}));

vi.mock("@shared/components/molecules/drop-down/blueprint", () => ({
  DropDown: (props: { children: React.ReactNode; content: React.ReactNode[] }) => (
    <div>
      {props.children}
      {props.content}
    </div>
  ),
}));

vi.mock("@shared/components/molecules/drop-down/blueprint/item", () => ({
  DropDownItem: (props: { children: React.ReactNode }) => <div>{props.children}</div>,
}));

vi.mock("@shared/components/atoms/icon/file-text", () => ({
  FileTextIcon: () => <span>FileText</span>,
}));

vi.mock("@shared/components/atoms/icon/ballpen", () => ({
  BallpenIcon: () => <span>Ballpen</span>,
}));

describe("components/molecules/drop-down/PostMenuDropDown", () => {
  it("記事だけの作成メニューを表示する", async () => {
    const { PostMenuDropDown } = await import(
      "@shared/components/molecules/drop-down/post-menu"
    );

    render(<PostMenuDropDown />);

    expect(screen.getByText("記事")).toBeInTheDocument();
    expect(screen.queryByText("メモ")).not.toBeInTheDocument();
    expect(screen.queryByText("連載")).not.toBeInTheDocument();
  });
});
