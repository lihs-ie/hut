const MS_PER_YEAR = 1000 * 60 * 60 * 24 * 365.2425;

export const formatDate = (date: Date): string => {
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  return `${year}/${month}/${day}`;
};

export const formatDateTime = (date: Date): string => {
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  const hours = String(date.getHours()).padStart(2, "0");
  const minutes = String(date.getMinutes()).padStart(2, "0");
  return `${year}/${month}/${day} ${hours}:${minutes}`;
};

export const determineExperience = (
  startedAt: Date,
  now: Date = new Date(),
): number => {
  const diffMs = now.getTime() - startedAt.getTime();

  if (diffMs <= 0) {
    return 0.0;
  }

  const years = diffMs / MS_PER_YEAR;

  return Math.round(years * 10) / 10;
};
