import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ModestText } from "@shared/components/atoms/text/modest";
import { GlobeIcon } from "@shared/components/atoms/icon";

const meta = {
  component: ModestText,
} satisfies Meta<typeof ModestText>;

export default meta;

export const Default: StoryObj<typeof ModestText> = {
  args: {
    children: (
      <>
        <GlobeIcon className="icon-sm" />
        &nbsp;This is a modest text.
      </>
    ),
  },
};
