import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminPrivacyEditTemplate } from "@shared/components/templates/admin/privacy";
import { EditablePolicySection } from "@shared/components/molecules/editor/policy-section";

const meta = {
  component: AdminPrivacyEditTemplate,
  parameters: {
    layout: "fullscreen",
  },
} satisfies Meta<typeof AdminPrivacyEditTemplate>;

export default meta;

type Story = StoryObj<typeof AdminPrivacyEditTemplate>;

const sampleSections: EditablePolicySection[] = [
  {
    headline: "1. 基本方針",
    body: "木漏れ日ブログ（以下「当ブログ」）は、訪問者の個人情報の保護を重要視し、個人情報保護法およびその他関連法令を遵守します。",
    list: null,
  },
  {
    headline: "2. 収集する情報",
    body: "当ブログでは、以下の情報を収集する場合があります：",
    list: [
      "アクセスログ：IPアドレス、ブラウザの種類、アクセス日時、参照元URLなど",
      "Cookie：テーマ設定（ダークモード/ライトモード）などのユーザー体験向上のための情報",
    ],
  },
  {
    headline: "3. 情報の利用目的",
    body: "収集した情報は以下の目的で利用します：",
    list: [
      "ブログサービスの提供と改善",
      "ユーザー設定の保存（テーマ設定など）",
      "アクセス統計の分析",
      "不正アクセスやセキュリティ対策",
    ],
  },
  {
    headline: "4. Cookieについて",
    body: "当ブログでは、ユーザー体験の向上のためにCookieを使用しています。Cookieは、お使いのブラウザ設定で無効にすることができますが、一部機能が正常に動作しない場合があります。",
    list: ["テーマ設定の保存（ダークモード/ライトモード）"],
  },
  {
    headline: "5. 第三者への提供",
    body: "当ブログは、法令に基づく場合を除き、ユーザーの同意なく第三者に個人情報を提供することはありません。",
    list: null,
  },
];

const mockOnSave = async (sections: EditablePolicySection[]) => {
  console.log("Saving sections:", sections);
  await new Promise((resolve) => setTimeout(resolve, 1000));
};

export const Default: Story = {
  args: {
    initialSections: sampleSections,
    onSave: mockOnSave,
  },
};

export const Empty: Story = {
  args: {
    initialSections: [
      {
        headline: "",
        body: "",
        list: null,
      },
    ],
    onSave: mockOnSave,
  },
};
