import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import { AvatarUpload } from "@shared/components/molecules/form/avatar-upload";

const meta = {
  component: AvatarUpload,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof AvatarUpload>;

export default meta;

type Story = StoryObj<typeof AvatarUpload>;

export const Default: Story = {
  args: {
    avatarUrl: "https://api.dicebear.com/7.x/avataaars/svg?seed=Felix",
    displayName: "山田 太郎",
    onAvatarChange: () => {},
  },
};

export const Interactive: Story = {
  render: () => {
    const [avatarUrl, setAvatarUrl] = useState(
      "https://api.dicebear.com/7.x/avataaars/svg?seed=Felix"
    );

    const handleAvatarChange = (file: File) => {
      const reader = new FileReader();
      reader.onloadend = () => {
        setAvatarUrl(reader.result as string);
      };
      reader.readAsDataURL(file);
    };

    return (
      <AvatarUpload
        avatarUrl={avatarUrl}
        displayName="山田 太郎"
        onAvatarChange={handleAvatarChange}
      />
    );
  },
};
