import { Timeline, timelineSchema } from "@/domains/common";
import { Factory } from "../../builder";

export type DateProperties = {
  value: Date;
};

export const DateFactory = Factory<Date, DateProperties>({
  instantiate: (properties) => new Date(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ??
      new Date(Date.UTC(2000 + (seed % 50), seed % 12, (seed % 28) + 1)),
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type TimelineProperties = {
  createdAt: Date;
  updatedAt: Date;
};

export const TimelineFactory = Factory<Timeline, TimelineProperties>({
  instantiate: (properties) =>
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
  retrieve: (instance) => ({
    createdAt: instance.createdAt,
    updatedAt: instance.updatedAt,
  }),
});
