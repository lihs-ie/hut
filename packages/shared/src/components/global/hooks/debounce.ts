const DEFAULT_DELAY = 300;

type DebouncedFunction<T extends (...arguments_: Parameters<T>) => void> = ((
  ...arguments_: Parameters<T>
) => void) & {
  cancel: () => void;
};

/**
 * React非依存の純粋なdebounce関数
 * Server/Client両方でimport可能（ただしServer側では実行不可）
 */
export const debounce = <T extends (...arguments_: Parameters<T>) => void>(
  callback: T,
  delay: number = DEFAULT_DELAY
): DebouncedFunction<T> => {
  let timeoutId: NodeJS.Timeout | null = null;

  const debouncedFunction = (...arguments_: Parameters<T>) => {
    if (timeoutId) {
      clearTimeout(timeoutId);
    }
    timeoutId = setTimeout(() => {
      callback(...arguments_);
    }, delay);
  };

  debouncedFunction.cancel = () => {
    if (timeoutId) {
      clearTimeout(timeoutId);
      timeoutId = null;
    }
  };

  return debouncedFunction as DebouncedFunction<T>;
};
