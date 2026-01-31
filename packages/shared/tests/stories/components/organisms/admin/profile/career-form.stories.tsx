import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import { ProfileCareerForm } from "@shared/components/organisms/admin/profile/career-form";
import type { CareerFormData } from "@shared/components/molecules/form/career-item";

const meta = {
  component: ProfileCareerForm,
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
} satisfies Meta<typeof ProfileCareerForm>;

export default meta;

type Story = StoryObj<typeof ProfileCareerForm>;

export const Empty: Story = {
  args: {
    careers: [],
    onAdd: () => {},
    onUpdate: () => {},
    onRemove: () => {},
    onReorder: () => {},
  },
};

export const SingleItem: Story = {
  args: {
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
    onAdd: () => {},
    onUpdate: () => {},
    onRemove: () => {},
    onReorder: () => {},
  },
};

export const MultipleItems: Story = {
  args: {
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
      {
        id: "2",
        company: "株式会社スタートアップ",
        role: "Frontend Engineer",
        startDate: "2020-04",
        endDate: "2023-03",
        description: "React/TypeScriptを使用したSPAの開発に従事。",
      },
      {
        id: "3",
        company: "株式会社システム開発",
        role: "Junior Developer",
        startDate: "2018-04",
        endDate: "2020-03",
        description: "Java/Springを使ったバックエンド開発を担当。",
      },
    ],
    onAdd: () => {},
    onUpdate: () => {},
    onRemove: () => {},
    onReorder: () => {},
  },
};

export const Interactive: Story = {
  render: () => {
    const [careers, setCareers] = useState<CareerFormData[]>([
      {
        id: "1",
        company: "株式会社テックイノベーション",
        role: "Senior Full Stack Developer",
        startDate: "2023-04",
        endDate: null,
        description:
          "Next.jsとReactを使った大規模Webアプリケーションの設計・開発をリード。",
      },
      {
        id: "2",
        company: "株式会社スタートアップ",
        role: "Frontend Engineer",
        startDate: "2020-04",
        endDate: "2023-03",
        description: "React/TypeScriptを使用したSPAの開発に従事。",
      },
    ]);

    const handleAdd = () => {
      const newCareer: CareerFormData = {
        id: Date.now().toString(),
        company: "",
        role: "",
        startDate: "",
        endDate: null,
        description: "",
      };
      setCareers((prev) => [newCareer, ...prev]);
    };

    const handleUpdate = (
      id: string,
      field: keyof CareerFormData,
      value: string | null
    ) => {
      setCareers((prev) =>
        prev.map((career) =>
          career.id === id ? { ...career, [field]: value } : career
        )
      );
    };

    const handleRemove = (id: string) => {
      setCareers((prev) => prev.filter((career) => career.id !== id));
    };

    const handleReorder = (fromIndex: number, toIndex: number) => {
      setCareers((prev) => {
        const newCareers = [...prev];
        const [draggedItem] = newCareers.splice(fromIndex, 1);
        newCareers.splice(toIndex, 0, draggedItem);
        return newCareers;
      });
    };

    return (
      <ProfileCareerForm
        careers={careers}
        onAdd={handleAdd}
        onUpdate={handleUpdate}
        onRemove={handleRemove}
        onReorder={handleReorder}
      />
    );
  },
};
