/**
 * タグシードデータ
 */

import { createDocument, TAG_IDS } from "./common";

/**
 * タグ名インデックスを作成する
 * パス: index/tags/name/{tagName}
 */
async function createTagNameIndex(
  tagName: string,
  tagIdentifier: string,
): Promise<void> {
  await createDocument(
    "index/tags/name",
    tagName,
    {
      referenceIdentifier: tagIdentifier,
      createdAt: new Date(),
    },
    { useTimestamp: true },
  );
}

export async function seedTags(): Promise<void> {
  console.log("\n--- Creating Tags ---");

  const now = new Date();

  const tags = [
    {
      id: TAG_IDS.typescript,
      name: "TypeScript",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/typescript/typescript-original.svg",
    },
    {
      id: TAG_IDS.react,
      name: "React",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/react/react-original.svg",
    },
    {
      id: TAG_IDS.nextjs,
      name: "Next.js",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/nextjs/nextjs-original.svg",
    },
    {
      id: TAG_IDS.go,
      name: "Go",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/go/go-original.svg",
    },
    {
      id: TAG_IDS.rust,
      name: "Rust",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/rust/rust-original.svg",
    },
  ];

  for (const tag of tags) {
    // タグドキュメントを作成
    await createDocument("tags", tag.id, {
      identifier: tag.id,
      name: tag.name,
      logo: tag.logo,
      timeline: {
        createdAt: now,
        updatedAt: now,
      },
      version: 1,
    });

    // タグ名インデックスを作成
    await createTagNameIndex(tag.name, tag.id);
  }
}
