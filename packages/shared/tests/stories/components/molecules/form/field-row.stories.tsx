import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { FormFieldRow } from "@shared/components/molecules/form/field-row";
import { FormField } from "@shared/components/molecules/form/field";
import { TextInput } from "@shared/components/atoms/input/text";
import { MonthInput } from "@shared/components/atoms/input/month";

const meta = {
  component: FormFieldRow,
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
} satisfies Meta<typeof FormFieldRow>;

export default meta;

type Story = StoryObj<typeof FormFieldRow>;

export const Default: Story = {
  args: {
    children: (
      <>
        <FormField label="GitHubユーザー名">
          <TextInput placeholder="username" />
        </FormField>
        <FormField label="Twitterユーザー名">
          <TextInput placeholder="@なしで入力" />
        </FormField>
      </>
    ),
  },
};

export const DateRange: Story = {
  args: {
    children: (
      <>
        <FormField label="開始日">
          <MonthInput value="2023-04" />
        </FormField>
        <FormField label="終了日">
          <MonthInput />
        </FormField>
      </>
    ),
  },
};
