import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import {
  CareerItem,
  type CareerFormData,
} from "@shared/components/molecules/form/career-item";

const meta = {
  component: CareerItem,
  parameters: {
    layout: "padded",
  },
  decorators: [
    (Story) => (
      <div style={{ maxWidth: "700px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof CareerItem>;

export default meta;

type Story = StoryObj<typeof CareerItem>;

export const Empty: Story = {
  args: {
    career: {
      id: "1",
      company: "",
      role: "",
      startDate: "",
      endDate: null,
      description: "",
    },
    index: 0,
    onUpdate: () => {},
    onRemove: () => {},
    onDragStart: () => {},
    onDragOver: () => {},
    onDrop: () => {},
  },
};

export const Current: Story = {
  args: {
    career: {
      id: "1",
      company: "株式会社テックイノベーション",
      role: "Senior Full Stack Developer",
      startDate: "2023-04",
      endDate: null,
      description:
        "Next.jsとReactを使った大規模Webアプリケーションの設計・開発をリード。",
    },
    index: 0,
    onUpdate: () => {},
    onRemove: () => {},
    onDragStart: () => {},
    onDragOver: () => {},
    onDrop: () => {},
  },
};

export const Past: Story = {
  args: {
    career: {
      id: "2",
      company: "株式会社スタートアップ",
      role: "Frontend Engineer",
      startDate: "2020-04",
      endDate: "2023-03",
      description: "React/TypeScriptを使用したSPAの開発に従事。",
    },
    index: 1,
    onUpdate: () => {},
    onRemove: () => {},
    onDragStart: () => {},
    onDragOver: () => {},
    onDrop: () => {},
  },
};

export const Interactive: Story = {
  render: () => {
    const [career, setCareer] = useState<CareerFormData>({
      id: "1",
      company: "株式会社テックイノベーション",
      role: "Senior Full Stack Developer",
      startDate: "2023-04",
      endDate: null,
      description:
        "Next.jsとReactを使った大規模Webアプリケーションの設計・開発をリード。",
    });

    const handleUpdate = (
      id: string,
      field: keyof CareerFormData,
      value: string | null
    ) => {
      setCareer((prev) => ({ ...prev, [field]: value }));
    };

    return (
      <CareerItem
        career={career}
        index={0}
        onUpdate={handleUpdate}
        onRemove={() => alert("削除がクリックされました")}
        onDragStart={() => {}}
        onDragOver={() => {}}
        onDrop={() => {}}
      />
    );
  },
};
