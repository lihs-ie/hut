import { Timestamp } from "firebase-admin/firestore";

export const adminProfile = {
  identifier: "01KJ1VZZ54X6JW632A8VEYRWAF",
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
          from: Timestamp.fromDate(new Date("2020-04-01")),
          to: null,
        },
        role: "Full Stack Developer",
        description:
          "Webアプリケーション開発をリード。フロントエンドからバックエンドまで担当。",
      },
      {
        company: "Startup Corp.",
        period: {
          from: Timestamp.fromDate(new Date("2018-04-01")),
          to: Timestamp.fromDate(new Date("2020-03-31")),
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
          tag: "01KJ1VZZ524TJ3GBYCZTAW4SD4",
          from: Timestamp.fromDate(new Date("2019-01-01")),
          continue: true,
          type: "both",
        },
        {
          tag: "01KJ1VZZ53SBTH5F8PM3X5TVF9",
          from: Timestamp.fromDate(new Date("2019-06-01")),
          continue: true,
          type: "both",
        },
        {
          tag: "01KJ1VZZ5379RVB0H2Y00CBYCX",
          from: Timestamp.fromDate(new Date("2020-01-01")),
          continue: true,
          type: "business",
        },
      ],
      backend: [
        {
          tag: "01KJ1VZZ54BRMJ2X1Q475BB416",
          from: Timestamp.fromDate(new Date("2020-06-01")),
          continue: true,
          type: "business",
        },
        {
          tag: "01KJ1VZZ54QX90Q0RXA8RE2H2S",
          from: Timestamp.fromDate(new Date("2022-01-01")),
          continue: true,
          type: "personal",
        },
      ],
    },
  },
  version: 1,
};
