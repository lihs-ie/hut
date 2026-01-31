import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import { ProfileTechStackForm } from "@shared/components/organisms/admin/profile/tech-stack-form";
import type { TechStackFormData } from "@shared/components/molecules/form/tech-stack-item";

const meta = {
  component: ProfileTechStackForm,
  parameters: {
    layout: "padded",
  },
  decorators: [
    (Story) => (
      <div style={{ maxWidth: "800px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof ProfileTechStackForm>;

export default meta;

type Story = StoryObj<typeof ProfileTechStackForm>;

export const Empty: Story = {
  args: {
    techStacks: [],
    onAdd: () => {},
    onUpdate: () => {},
    onRemove: () => {},
  },
};

export const SingleItem: Story = {
  args: {
    techStacks: [
      {
        id: "1",
        technology: "React",
        startedAt: "2020-01",
        isActive: true,
        experienceYears: 4,
        experienceType: "both",
      },
    ],
    onAdd: () => {},
    onUpdate: () => {},
    onRemove: () => {},
  },
};

export const MultipleItems: Story = {
  args: {
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
      {
        id: "3",
        technology: "TypeScript",
        startedAt: "2019-03",
        isActive: true,
        experienceYears: 5,
        experienceType: "both",
      },
    ],
    onAdd: () => {},
    onUpdate: () => {},
    onRemove: () => {},
  },
};

export const Interactive: Story = {
  render: () => {
    const [techStacks, setTechStacks] = useState<TechStackFormData[]>([
      {
        id: "1",
        technology: "React",
        startedAt: "2020-01",
        isActive: true,
        experienceYears: 4,
        experienceType: "both",
      },
    ]);

    const handleAdd = () => {
      const newTech: TechStackFormData = {
        id: Date.now().toString(),
        technology: "",
        startedAt: "",
        isActive: true,
        experienceYears: 0,
        experienceType: "personal",
      };
      setTechStacks((prev) => [...prev, newTech]);
    };

    const handleUpdate = (
      id: string,
      field: keyof TechStackFormData,
      value: string | boolean | number
    ) => {
      setTechStacks((prev) =>
        prev.map((tech) =>
          tech.id === id ? { ...tech, [field]: value } : tech
        )
      );
    };

    const handleRemove = (id: string) => {
      setTechStacks((prev) => prev.filter((tech) => tech.id !== id));
    };

    return (
      <ProfileTechStackForm
        techStacks={techStacks}
        onAdd={handleAdd}
        onUpdate={handleUpdate}
        onRemove={handleRemove}
      />
    );
  },
};
