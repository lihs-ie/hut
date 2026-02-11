/**
 * 管理者プロフィールシードデータ
 */

import { createDocument, ADMIN_ID, TAG_IDS } from "./common";

export async function seedAdmin(): Promise<void> {
  console.log("\n--- Creating Admin Profile ---");

  const admin = {
    identifier: ADMIN_ID,
    profile: {
      avatar:
        "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?w=400",
      name: "Taro Yamada",
      email: "taro@example.com",
      bio: "フルスタックエンジニア。TypeScript、React、Go、Rustを主に使用。クリーンアーキテクチャとDDDに興味があります。",
      careers: [
        {
          company: "Tech Company Inc.",
          period: {
            from: new Date("2020-04-01"),
            to: null,
          },
          role: "Full Stack Developer",
          description:
            "Webアプリケーション開発をリード。フロントエンドからバックエンドまで担当。",
        },
        {
          company: "Startup Corp.",
          period: {
            from: new Date("2018-04-01"),
            to: new Date("2020-03-31"),
          },
          role: "Full Stack Developer",
          description: "新規プロダクトの立ち上げに従事。",
        },
      ],
      externalServices: [
        {
          type: "gitHub",
          user: "taro-yamada",
        },
        {
          type: "x",
          user: "taro_dev",
        },
      ],
      techStacks: {
        frontend: [
          {
            tag: TAG_IDS.typescript,
            from: new Date("2019-01-01"),
            continue: true,
            type: "both",
          },
          {
            tag: TAG_IDS.react,
            from: new Date("2019-06-01"),
            continue: true,
            type: "both",
          },
          {
            tag: TAG_IDS.nextjs,
            from: new Date("2020-01-01"),
            continue: true,
            type: "business",
          },
        ],
        backend: [
          {
            tag: TAG_IDS.go,
            from: new Date("2020-06-01"),
            continue: true,
            type: "business",
          },
          {
            tag: TAG_IDS.rust,
            from: new Date("2022-01-01"),
            continue: true,
            type: "personal",
          },
        ],
      },
    },
  };

  await createDocument(
    "admin",
    "admin",
    {
      identifier: admin.identifier,
      profile: admin.profile,
      version: 1,
    },
    { useTimestamp: true },
  );
}
