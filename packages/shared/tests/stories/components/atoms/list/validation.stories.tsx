import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ValidationErrorList } from "@shared/components/atoms/list/validation";
import { ValidationError, validationError } from "@shared/aspects/error";

const meta = {
  component: ValidationErrorList,
} satisfies Meta<typeof ValidationErrorList>;

export default meta;

const createError = (seed: number): ValidationError =>
  validationError(`field_${seed}`, `This is error description for field ${seed}`);

export const Default: StoryObj<typeof ValidationErrorList> = {
  args: {
    errors: [createError(1), createError(2), createError(3)],
  },
};
