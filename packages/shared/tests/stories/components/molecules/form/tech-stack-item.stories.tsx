import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import {
  TechStackItem,
  type TechStackFormData,
} from "@shared/components/molecules/form/tech-stack-item";

const meta = {
  component: TechStackItem,
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
} satisfies Meta<typeof TechStackItem>;

export default meta;

type Story = StoryObj<typeof TechStackItem>;

const AVAILABLE_TECHNOLOGIES = [
  "React",
  "Next.js",
  "TypeScript",
  "JavaScript",
  "Node.js",
  "Go",
  "Python",
];

export const Empty: Story = {
  args: {
    techStack: {
      id: "1",
      technology: "",
      startedAt: "",
      isActive: true,
      experienceYears: 0,
      experienceType: "personal",
    },
    onUpdate: () => {},
    onRemove: () => {},
    availableTechnologies: AVAILABLE_TECHNOLOGIES,
  },
};

export const Filled: Story = {
  args: {
    techStack: {
      id: "1",
      technology: "React",
      startedAt: "2020-01",
      isActive: true,
      experienceYears: 4,
      experienceType: "both",
    },
    onUpdate: () => {},
    onRemove: () => {},
    availableTechnologies: AVAILABLE_TECHNOLOGIES,
  },
};

export const Inactive: Story = {
  args: {
    techStack: {
      id: "2",
      technology: "Vue.js",
      startedAt: "2018-06",
      isActive: false,
      experienceYears: 2,
      experienceType: "professional",
    },
    onUpdate: () => {},
    onRemove: () => {},
    availableTechnologies: [...AVAILABLE_TECHNOLOGIES, "Vue.js"],
  },
};

export const Interactive: Story = {
  render: () => {
    const [techStack, setTechStack] = useState<TechStackFormData>({
      id: "1",
      technology: "React",
      startedAt: "2020-01",
      isActive: true,
      experienceYears: 4,
      experienceType: "both",
    });

    const handleUpdate = (
      id: string,
      field: keyof TechStackFormData,
      value: string | boolean | number
    ) => {
      setTechStack((prev) => ({ ...prev, [field]: value }));
    };

    return (
      <TechStackItem
        techStack={techStack}
        onUpdate={handleUpdate}
        onRemove={() => alert("削除がクリックされました")}
        availableTechnologies={AVAILABLE_TECHNOLOGIES}
      />
    );
  },
};
