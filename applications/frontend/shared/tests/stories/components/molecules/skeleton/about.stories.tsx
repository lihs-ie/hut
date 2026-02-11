import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ProfileSkeleton, TechStackSkeleton, CareerSkeleton } from "@shared/components/molecules/skeleton/about";

const meta = {
  component: ProfileSkeleton,
} satisfies Meta<typeof ProfileSkeleton>;

export default meta;

export const Profile: StoryObj<typeof ProfileSkeleton> = {};

export const TechStack: StoryObj = {
  render: () => <TechStackSkeleton />,
};

export const Career: StoryObj = {
  render: () => <CareerSkeleton />,
};
