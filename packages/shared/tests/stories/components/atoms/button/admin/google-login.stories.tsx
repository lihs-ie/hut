import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { GoogleLoginButton } from "../../../../../../../admin/src/app/admin/_components/atoms/button/google-login";

const meta = {
  component: GoogleLoginButton,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof GoogleLoginButton>;

export default meta;

type Story = StoryObj<typeof GoogleLoginButton>;

const mockLogin = async () => {
  await new Promise((resolve) => setTimeout(resolve, 1000));
  console.log("Login attempted");
};

export const Default: Story = {
  args: {
    login: mockLogin,
  },
};

export const WithCustomText: Story = {
  args: {
    login: mockLogin,
    children: "Googleでサインイン",
  },
};

export const Disabled: Story = {
  args: {
    login: mockLogin,
    disabled: true,
  },
};

export const DisabledWithCustomText: Story = {
  args: {
    login: mockLogin,
    children: "認証中...",
    disabled: true,
  },
};
