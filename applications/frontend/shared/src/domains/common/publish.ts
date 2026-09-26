export type Published<T extends { publishedAt: Date | null }> = T & {
  publishedAt: Date;
};

export const isPublished = <T extends { publishedAt: Date | null }>(
  entity: T,
): entity is Published<T> => {
  const { publishedAt } = entity;

  return publishedAt instanceof Date && !Number.isNaN(publishedAt.getTime());
};
