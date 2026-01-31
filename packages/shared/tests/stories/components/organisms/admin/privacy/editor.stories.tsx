import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { PolicyEditorPresenter } from "../../../../../../../admin/src/app/admin/_components/organisms/admin/privacy/editor.presenter";
import { UnvalidatedPrivacyPolicy } from "@shared/domains/document";

const meta = {
  component: PolicyEditorPresenter,
  parameters: {
    layout: "padded",
  },
} satisfies Meta<typeof PolicyEditorPresenter>;

export default meta;

type Story = StoryObj<typeof PolicyEditorPresenter>;

const createPrivacyPolicy = (
  sections: UnvalidatedPrivacyPolicy["sections"]
): UnvalidatedPrivacyPolicy => ({
  sections,
  timeline: {
    createdAt: new Date("2024-01-01T00:00:00"),
    updatedAt: new Date("2024-01-15T10:30:00"),
  },
});

const mockPersist = async (unvalidated: UnvalidatedPrivacyPolicy) => {
  console.log("Persisting privacy policy:", unvalidated);
  await new Promise((resolve) => setTimeout(resolve, 1000));
};

export const Default: Story = {
  args: {
    value: createPrivacyPolicy([
      {
        headline: "個人情報の収集について",
        body: "当サービスでは、サービスの提供にあたり、以下の個人情報を収集することがあります。",
        list: ["氏名", "メールアドレス", "利用履歴"],
      },
      {
        headline: "個人情報の利用目的",
        body: "収集した個人情報は、以下の目的で利用いたします。",
        list: ["サービスの提供・運営のため", "お問い合わせへの対応のため", "利用規約に違反したユーザーの特定のため"],
      },
    ]),
    persist: mockPersist,
  },
};

export const SingleSection: Story = {
  args: {
    value: createPrivacyPolicy([
      {
        headline: "プライバシーポリシー",
        body: "当サービスは、ユーザーのプライバシーを尊重し、個人情報の保護に努めます。",
        list: null,
      },
    ]),
    persist: mockPersist,
  },
};

export const MultipleSections: Story = {
  args: {
    value: createPrivacyPolicy([
      {
        headline: "はじめに",
        body: "本プライバシーポリシーは、当サービスにおける個人情報の取り扱いについて説明します。",
        list: null,
      },
      {
        headline: "収集する情報",
        body: "当サービスでは以下の情報を収集します。",
        list: ["アカウント情報", "利用データ", "Cookie情報"],
      },
      {
        headline: "情報の利用",
        body: "収集した情報は以下の目的で利用します。",
        list: ["サービス提供", "サービス改善", "カスタマーサポート"],
      },
      {
        headline: "情報の共有",
        body: "当サービスは、法令に基づく場合を除き、ユーザーの同意なく第三者に個人情報を提供しません。",
        list: null,
      },
      {
        headline: "お問い合わせ",
        body: "プライバシーに関するお問い合わせは、support@example.comまでご連絡ください。",
        list: null,
      },
    ]),
    persist: mockPersist,
  },
};

export const WithLongContent: Story = {
  args: {
    value: createPrivacyPolicy([
      {
        headline: "長いコンテンツを含むセクション",
        body: `当サービスは、ユーザーの皆様のプライバシーを最優先事項として考えております。
本ポリシーでは、当サービスがどのような情報を収集し、どのように利用するかについて詳細に説明いたします。

当サービスを利用することにより、本ポリシーに記載された内容に同意したものとみなされます。
ご不明な点がございましたら、お気軽にお問い合わせください。

なお、本ポリシーは予告なく変更される場合があります。
変更があった場合は、当サービス上で通知いたします。`,
        list: [
          "利用者のアカウント情報（メールアドレス、ユーザー名など）",
          "サービス利用履歴およびログデータ",
          "デバイス情報およびブラウザ情報",
          "IPアドレスおよび位置情報",
          "Cookieおよびトラッキング技術による情報",
        ],
      },
    ]),
    persist: mockPersist,
  },
};

export const Empty: Story = {
  args: {
    value: createPrivacyPolicy([
      {
        headline: "",
        body: "",
        list: null,
      },
    ]),
    persist: mockPersist,
  },
};
