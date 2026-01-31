import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { CareerCard } from "@shared/components/molecules/list/card/career";
import { Forger } from "@lihs-ie/forger-ts";
import {
  CareerMold,
  PeriodMold,
} from "../../../../../support/molds/domains/user";

const meta = {
  component: CareerCard,
} satisfies Meta<typeof CareerCard>;

export default meta;

export const Default: StoryObj<typeof CareerCard> = {
  args: {
    career: Forger(CareerMold).forge({
      period: Forger(PeriodMold).forge({
        from: new Date("2020-01-01"),
        to: new Date("2022-12-31"),
      }),
    }),
  },
};

export const WithoutEndDate: StoryObj<typeof CareerCard> = {
  args: {
    career: Forger(CareerMold).forge({
      period: Forger(PeriodMold).forge({
        from: new Date("2020-01-01"),
        to: null,
      }),
    }),
  },
};
