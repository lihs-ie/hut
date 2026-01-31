import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import {
  ProfileEditForm,
  type ProfileFormData,
} from "@shared/components/organisms/admin/profile/edit-form";

const meta = {
  component: ProfileEditForm,
  parameters: {
    layout: "padded",
  },
  decorators: [
    (Story) => (
      <div style={{ maxWidth: "800px", margin: "0 auto" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof ProfileEditForm>;

export default meta;

type Story = StoryObj<typeof ProfileEditForm>;

const EMPTY_DATA: ProfileFormData = {
  avatarUrl: "https://api.dicebear.com/7.x/avataaars/svg?seed=Empty",
  displayName: "",
  bio: "",
  githubUsername: "",
  twitterUsername: "",
  website: "",
  techStacks: [],
  careers: [],
};

const FILLED_DATA: ProfileFormData = {
  avatarUrl: "https://api.dicebear.com/7.x/avataaars/svg?seed=Felix",
  displayName: "山田 太郎",
  bio: "PureScriptとHaskell、そして猫が好きなエンジニアです。",
  githubUsername: "yamada-taro",
  twitterUsername: "yamada_dev",
  website: "https://yamada.dev",
  techStacks: [
    {
      id: "1",
      technology: "React",
      startedAt: "2020-01",
      isActive: true,
      experienceYears: 4,
      experienceType: "both",
    },
    {
      id: "2",
      technology: "Next.js",
      startedAt: "2021-06",
      isActive: true,
      experienceYears: 2.5,
      experienceType: "professional",
    },
  ],
  careers: [
    {
      id: "1",
      company: "株式会社テックイノベーション",
      role: "Senior Full Stack Developer",
      startDate: "2023-04",
      endDate: null,
      description:
        "Next.jsとReactを使った大規模Webアプリケーションの設計・開発をリード。",
    },
  ],
};

export const Empty: Story = {
  args: {
    initialData: EMPTY_DATA,
    onSubmit: async (data) => {
      console.log("Submitted:", data);
    },
  },
};

export const Filled: Story = {
  args: {
    initialData: FILLED_DATA,
    onSubmit: async (data) => {
      console.log("Submitted:", data);
    },
  },
};

export const Interactive: Story = {
  render: () => {
    const [submitted, setSubmitted] = useState<ProfileFormData | null>(null);

    const handleSubmit = async (data: ProfileFormData) => {
      await new Promise((resolve) => setTimeout(resolve, 1000));
      setSubmitted(data);
      console.log("Submitted:", data);
    };

    return (
      <div>
        <ProfileEditForm initialData={FILLED_DATA} onSubmit={handleSubmit} />
        {submitted && (
          <div
            style={{
              marginTop: "2rem",
              padding: "1rem",
              background: "var(--secondary)",
              borderRadius: "var(--radius-md)",
            }}
          >
            <h3 style={{ margin: "0 0 0.5rem" }}>送信されたデータ:</h3>
            <pre style={{ margin: 0, fontSize: "0.75rem", overflow: "auto" }}>
              {JSON.stringify(submitted, null, 2)}
            </pre>
          </div>
        )}
      </div>
    );
  },
};
