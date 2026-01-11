import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ExternalServiceLinks } from "@shared/components/molecules/link/external-services";
import { Forger } from "@lihs-ie/forger-ts";
import { ExternalServiceMold } from "../../../../support/molds/domains/common/service";

const meta = {
  component: ExternalServiceLinks,
} satisfies Meta<typeof ExternalServiceLinks>;
export default meta;

export const Default: StoryObj<typeof ExternalServiceLinks> = {
  args: {
    services: [Forger(ExternalServiceMold).forge()],
  },
};
