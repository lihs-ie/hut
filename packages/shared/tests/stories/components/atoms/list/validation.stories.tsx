import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ValidationErrorList } from "@shared/components/atoms/list/validation";
import { ValidationError } from "@shared/aspects/error";
import { Builder, StringFactory } from "../../../../support/molds";

const meta = {
  component: ValidationErrorList,
} satisfies Meta<typeof ValidationErrorList>;

export default meta;

const createError = (): ValidationError => ({
  field: Builder(StringFactory(1, 10)).build(),
  description: Builder(StringFactory(10, 50)).build(),
  _tag: Symbol("ValidationError"),
});

export const Default: StoryObj<typeof ValidationErrorList> = {
  args: {
    errors: [createError(), createError(), createError()],
  },
};
