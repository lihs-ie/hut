import { PublishStatus } from "./status";

export type Published<T extends { publishedAt: Date | null | undefined }> = T & {
  publishedAt: Date;
};

export const isPublished = <T extends { publishedAt: Date | null | undefined }>(
  entity: T,
): entity is Published<T> => {
  return entity.publishedAt != null;
};

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
