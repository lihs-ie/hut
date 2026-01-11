/**
 * SiteDocumentシードデータ
 */

import { createDocument } from "./common";

export async function seedSiteDocument(): Promise<void> {
  console.log("\n--- Creating Site Document ---");

  const now = new Date();

  const siteDocument = {
    privacy: {
      sections: [
        {
          headline: "はじめに",
          body: "本プライバシーポリシーは、当サイト（以下「本サイト」といいます）における個人情報の取り扱いについて説明するものです。",
          list: null,
        },
        {
          headline: "収集する情報",
          body: "本サイトでは、以下の情報を収集する場合があります。",
          list: [
            "アクセスログ（IPアドレス、ブラウザ情報、アクセス日時等）",
            "お問い合わせフォームから送信された情報（氏名、メールアドレス、お問い合わせ内容等）",
            "Cookieを通じて取得される情報",
          ],
        },
        {
          headline: "情報の利用目的",
          body: "収集した情報は、以下の目的で利用します。",
          list: [
            "本サイトの運営・改善",
            "お問い合わせへの対応",
            "統計データの作成（個人を特定できない形式）",
          ],
        },
        {
          headline: "第三者への提供",
          body: "法令に基づく場合を除き、収集した個人情報を第三者に提供することはありません。",
          list: null,
        },
        {
          headline: "Cookieについて",
          body: "本サイトでは、ユーザー体験の向上やアクセス解析のためにCookieを使用しています。ブラウザの設定によりCookieを無効にすることも可能ですが、一部の機能が正常に動作しない場合があります。",
          list: null,
        },
        {
          headline: "お問い合わせ",
          body: "プライバシーポリシーに関するお問い合わせは、サイト内のお問い合わせフォームよりご連絡ください。",
          list: null,
        },
      ],
      timeline: {
        createdAt: now,
        updatedAt: now,
      },
    },
    version: 1,
  };

  await createDocument("site-documents", "site-document", siteDocument, {
    useTimestamp: true,
  });
}
