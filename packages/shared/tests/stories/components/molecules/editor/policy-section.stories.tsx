import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import {
  PolicySectionEditor,
  EditablePolicySection,
} from "@shared/components/molecules/editor/policy-section";

const meta = {
  component: PolicySectionEditor,
  parameters: {
    layout: "padded",
  },
} satisfies Meta<typeof PolicySectionEditor>;

export default meta;

type Story = StoryObj<typeof PolicySectionEditor>;

export const Empty: Story = {
  args: {
    section: {
      headline: "",
      body: "",
      list: null,
    },
    onUpdate: () => {},
    onDelete: () => {},
  },
};

export const WithContent: Story = {
  args: {
    section: {
      headline: "1. 基本方針",
      body: "木漏れ日ブログ（以下「当ブログ」）は、訪問者の個人情報の保護を重要視し、個人情報保護法およびその他関連法令を遵守します。",
      list: null,
    },
    onUpdate: () => {},
    onDelete: () => {},
  },
};

export const WithList: Story = {
  args: {
    section: {
      headline: "2. 収集する情報",
      body: "当ブログでは、以下の情報を収集する場合があります：",
      list: [
        "アクセスログ：IPアドレス、ブラウザの種類、アクセス日時、参照元URLなど",
        "Cookie：テーマ設定（ダークモード/ライトモード）などのユーザー体験向上のための情報",
      ],
    },
    onUpdate: () => {},
    onDelete: () => {},
  },
};

export const WithoutDeleteButton: Story = {
  args: {
    section: {
      headline: "唯一のセクション",
      body: "このセクションは削除できません。",
      list: null,
    },
    onUpdate: () => {},
    onDelete: () => {},
    showDelete: false,
  },
};

export const Interactive: Story = {
  render: () => {
    const [section, setSection] = useState<EditablePolicySection>({
      headline: "セクションタイトル",
      body: "セクションの内容を入力してください。",
      list: null,
    });

    return (
      <div style={{ maxWidth: "800px" }}>
        <PolicySectionEditor
          section={section}
          onUpdate={setSection}
          onDelete={() => alert("削除がクリックされました")}
        />
        <details style={{ marginTop: "1rem" }}>
          <summary style={{ cursor: "pointer", color: "var(--muted-foreground)" }}>
            現在の値
          </summary>
          <pre style={{
            marginTop: "0.5rem",
            padding: "1rem",
            backgroundColor: "var(--muted)",
            borderRadius: "0.5rem",
            fontSize: "0.75rem",
            overflow: "auto"
          }}>
            {JSON.stringify(section, null, 2)}
          </pre>
        </details>
      </div>
    );
  },
};
