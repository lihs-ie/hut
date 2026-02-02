import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { PrivacyIndex } from "@shared/components/templates/legal/privacy";
import { Forger } from "@lihs-ie/forger-ts";
import {
  PrivacyPolicyMold,
  PrivacyPolicySectionMold,
} from "../../../../support/molds/domains/document";
import { TimelineMold } from "../../../../support/molds/domains/common/date";

const meta = {
  component: PrivacyIndex,
} satisfies Meta<typeof PrivacyIndex>;

export default meta;

const getPrivacy = async () => Forger(PrivacyPolicyMold).forge();

export const Default: StoryObj<typeof PrivacyIndex> = {
  args: {
    getPrivacy,
  },
};

const getPrivacyWithManySections = async () =>
  Forger(PrivacyPolicyMold).forge({
    sections: [
      Forger(PrivacyPolicySectionMold).forge({
        headline: "個人情報の収集について",
        body: "当サイトでは、お問い合わせやサービスのご利用時に、お名前、メールアドレス等の個人情報を収集することがあります。",
        list: null,
      }),
      Forger(PrivacyPolicySectionMold).forge({
        headline: "個人情報の利用目的",
        body: "収集した個人情報は、以下の目的で利用いたします。",
        list: [
          "サービスの提供・運営のため",
          "お問い合わせに対する回答のため",
          "サービス改善のための分析のため",
        ],
      }),
      Forger(PrivacyPolicySectionMold).forge({
        headline: "個人情報の第三者提供",
        body: "当サイトでは、法令に基づく場合を除き、ご本人の同意なく個人情報を第三者に提供することはありません。",
        list: null,
      }),
      Forger(PrivacyPolicySectionMold).forge({
        headline: "Cookieの使用について",
        body: "当サイトでは、ユーザー体験の向上およびアクセス解析のためにCookieを使用しています。",
        list: [
          "セッション管理のため",
          "アクセス解析のため",
          "広告配信の最適化のため",
        ],
      }),
      Forger(PrivacyPolicySectionMold).forge({
        headline: "プライバシーポリシーの変更",
        body: "本ポリシーは、必要に応じて変更されることがあります。変更後のポリシーは、当サイトに掲載した時点から効力を生じるものとします。",
        list: null,
      }),
    ],
    timeline: Forger(TimelineMold).forge({
      createdAt: new Date("2024-01-01"),
      updatedAt: new Date("2024-06-01"),
    }),
  });

export const WithManySections: StoryObj<typeof PrivacyIndex> = {
  args: {
    getPrivacy: getPrivacyWithManySections,
  },
};
