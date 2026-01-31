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
    // Frontend
    {
      id: TAG_IDS.typescript,
      name: "TypeScript",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/typescript/typescript-original.svg",
    },
    {
      id: TAG_IDS.javascript,
      name: "JavaScript",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/javascript/javascript-original.svg",
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
      id: TAG_IDS.vue,
      name: "Vue.js",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/vuejs/vuejs-original.svg",
    },
    {
      id: TAG_IDS.nuxt,
      name: "Nuxt.js",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/nuxtjs/nuxtjs-original.svg",
    },
    {
      id: TAG_IDS.angular,
      name: "Angular",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/angular/angular-original.svg",
    },
    {
      id: TAG_IDS.svelte,
      name: "Svelte",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/svelte/svelte-original.svg",
    },
    {
      id: TAG_IDS.tailwindcss,
      name: "Tailwind CSS",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/tailwindcss/tailwindcss-original.svg",
    },
    {
      id: TAG_IDS.css,
      name: "CSS",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/css3/css3-original.svg",
    },
    {
      id: TAG_IDS.html,
      name: "HTML",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/html5/html5-original.svg",
    },
    // Backend
    {
      id: TAG_IDS.nodejs,
      name: "Node.js",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/nodejs/nodejs-original.svg",
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
    {
      id: TAG_IDS.python,
      name: "Python",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/python/python-original.svg",
    },
    {
      id: TAG_IDS.java,
      name: "Java",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/java/java-original.svg",
    },
    {
      id: TAG_IDS.php,
      name: "PHP",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/php/php-original.svg",
    },
    {
      id: TAG_IDS.ruby,
      name: "Ruby",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/ruby/ruby-original.svg",
    },
    {
      id: TAG_IDS.csharp,
      name: "C#",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/csharp/csharp-original.svg",
    },
    {
      id: TAG_IDS.kotlin,
      name: "Kotlin",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/kotlin/kotlin-original.svg",
    },
    // Database
    {
      id: TAG_IDS.postgresql,
      name: "PostgreSQL",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/postgresql/postgresql-original.svg",
    },
    {
      id: TAG_IDS.mysql,
      name: "MySQL",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/mysql/mysql-original.svg",
    },
    {
      id: TAG_IDS.mongodb,
      name: "MongoDB",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/mongodb/mongodb-original.svg",
    },
    {
      id: TAG_IDS.redis,
      name: "Redis",
      logo: "https://raw.githubusercontent.com/devicons/devicon/master/icons/redis/redis-original.svg",
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
