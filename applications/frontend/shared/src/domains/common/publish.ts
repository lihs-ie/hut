export type Published<T extends { publishedAt: Date | null | undefined }> = T & {
  publishedAt: Date;
};

export const isPublished = <T extends { publishedAt: Date | null | undefined }>(
  entity: T,
): entity is Published<T> => {
  const { publishedAt } = entity;

  return publishedAt instanceof Date && !Number.isNaN(publishedAt.getTime());
};
