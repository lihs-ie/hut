const normalize = (text: string): string =>
  text
    .toLowerCase()
    .replace(/\s+/g, "")
    .replace(/[^\p{L}\p{N}]/gu, "");

export const generateNgrams = (text: string, min = 2, max = 4): string[] => {
  const normalized = normalize(text);
  const ngrams: Set<string> = new Set();

  for (let n = min; n <= max; n++) {
    for (let i = 0; i <= normalized.length - n; i++) {
      ngrams.add(normalized.slice(i, i + n));
    }
  }

  return Array.from(ngrams);
};

type BonusContext<T> = {
  targetSet: Set<string>;
  field: T;
};

type Bonus<T> = (ngram: string, context: BonusContext<T>) => number;

export type Scorer<T> = (ngrams: string[], target: string, field: T) => number;

export const score =
  <T>(bonus: Bonus<T>) =>
  (
    ngrams: string[],
    target: string,
    field: BonusContext<T>["field"]
  ): number => {
    const targetSet = new Set(target);

    let total = 0;
    for (const ngram of ngrams) {
      total += bonus(ngram, { targetSet, field });
    }

    return total;
  };
