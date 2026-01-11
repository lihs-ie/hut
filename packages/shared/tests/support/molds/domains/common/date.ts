import { Timeline, timelineSchema } from "@shared/domains/common";
import { Mold } from "@lihs-ie/forger-ts";

export type DateProperties = {
  value: Date;
};

export const DateMold = Mold<Date, DateProperties>({
  pour: (properties) => new Date(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ??
      new Date(Date.UTC(2000 + (seed % 50), seed % 12, (seed % 28) + 1)),
  }),
});

export type TimelineProperties = {
  createdAt: Date;
  updatedAt: Date;
};

export const TimelineMold = Mold<Timeline, TimelineProperties>({
  pour: (properties) =>
    timelineSchema.parse({
      createdAt: properties.createdAt,
      updatedAt: properties.updatedAt,
    }),
  prepare: (overrides, seed) => {
    const createdAt =
      overrides.createdAt ??
      new Date(Date.UTC(2000 + (seed % 50), seed % 12, (seed % 28) + 1));
    const updatedAt =
      overrides.updatedAt ??
      new Date(
        Date.UTC(
          2000 + (seed % 50),
          seed % 12,
          Math.min((seed % 28) + 1 + (seed % 5), 28)
        )
      );

    return {
      createdAt,
      updatedAt,
    };
  },
});

export const TimelineFactory = TimelineMold;
