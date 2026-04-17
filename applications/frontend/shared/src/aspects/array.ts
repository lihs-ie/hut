export const FIRESTORE_IN_BATCH_LIMIT = 30;

export const chunk = <T>(items: T[], size: number): T[][] => {
  if (size <= 0) {
    throw new Error(`chunk size must be greater than 0, got ${size}`);
  }

  if (items.length === 0) {
    return [];
  }

  const result: T[][] = [];

  for (let index = 0; index < items.length; index += size) {
    result.push(items.slice(index, index + size));
  }

  return result;
};
