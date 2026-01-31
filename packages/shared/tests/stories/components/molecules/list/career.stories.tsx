import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { CareerList } from "@shared/components/molecules/list/career";
import { Forger } from "@lihs-ie/forger-ts";
import { CareerMold, PeriodMold } from "../../../../support/molds/domains/user";

const meta = {
  component: CareerList,
} satisfies Meta<typeof CareerList>;

export default meta;

const lastPeriod = Forger(PeriodMold).forge({
  from: new Date("2020-01-01"),
  to: null,
});

const periods = Forger(PeriodMold)
  .forgeMulti(5, {
    from: new Date("2015-01-01"),
    to: new Date("2019-12-31"),
  })
  .concat([lastPeriod]);

const careers = periods.map((period, index) =>
  Forger(CareerMold).forgeWithSeed(index, {
    period,
  })
);

export const Default: StoryObj<typeof CareerList> = {
  args: {
    careers,
  },
};
