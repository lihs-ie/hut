import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import {
  ProfileBasicInfoForm,
  type BasicInfo,
} from "@shared/components/organisms/admin/profile/basic-info-form";

const meta = {
  component: ProfileBasicInfoForm,
  parameters: {
    layout: "padded",
  },
  decorators: [
    (Story) => (
      <div style={{ maxWidth: "600px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof ProfileBasicInfoForm>;

export default meta;

type Story = StoryObj<typeof ProfileBasicInfoForm>;

export const Empty: Story = {
  args: {
    basicInfo: {
      displayName: "",
      bio: "",
      githubUsername: "",
      twitterUsername: "",
      website: "",
    },
    onUpdate: () => {},
  },
};

export const Filled: Story = {
  args: {
    basicInfo: {
      displayName: "山田 太郎",
      bio: "PureScriptとHaskell、そして猫が好きなエンジニアです。",
      githubUsername: "yamada-taro",
      twitterUsername: "yamada_dev",
      website: "https://yamada.dev",
    },
    onUpdate: () => {},
  },
};

export const PartiallyFilled: Story = {
  args: {
    basicInfo: {
      displayName: "山田 太郎",
      bio: "",
      githubUsername: "yamada-taro",
      twitterUsername: "",
      website: "",
    },
    onUpdate: () => {},
  },
};

export const Interactive: Story = {
  render: () => {
    const [basicInfo, setBasicInfo] = useState<BasicInfo>({
      displayName: "山田 太郎",
      bio: "PureScriptとHaskell、そして猫が好きなエンジニアです。",
      githubUsername: "",
      twitterUsername: "",
      website: "",
    });

    const handleUpdate = <K extends keyof BasicInfo>(
      field: K,
      value: BasicInfo[K]
    ) => {
      setBasicInfo((prev) => ({ ...prev, [field]: value }));
    };

    return (
      <ProfileBasicInfoForm basicInfo={basicInfo} onUpdate={handleUpdate} />
    );
  },
};
