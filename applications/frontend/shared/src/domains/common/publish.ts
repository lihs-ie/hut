import { PublishStatus } from "./status";

export type ComputePublishedAtArguments = {
  currentStatus: PublishStatus | null;
  nextStatus: PublishStatus;
  currentPublishedAt: Date | null;
  now: Date;
};

export const computePublishedAt = (
  args: ComputePublishedAtArguments,
): Date | null => {
  if (args.currentPublishedAt !== null) {
    return args.currentPublishedAt;
  }

  if (args.nextStatus === PublishStatus.PUBLISHED) {
    return args.now;
  }

  return null;
};
