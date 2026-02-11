import { useState, useEffect, useRef, useCallback } from "react";

const DEFAULT_DELAY = 300;

export const useDebounce = <T>(value: T, delay: number = DEFAULT_DELAY): T => {
  const [debouncedValue, setDebouncedValue] = useState<T>(value);

  useEffect(() => {
    const timer = setTimeout(() => {
      setDebouncedValue(value);
    }, delay);

    return () => {
      clearTimeout(timer);
    };
  }, [value, delay]);

  return debouncedValue;
};

export const useDebouncedCallback = <T extends (...arguments_: Parameters<T>) => void>(
  callback: T,
  delay: number = DEFAULT_DELAY
): T => {
  const timerRef = useRef<NodeJS.Timeout | null>(null);

  useEffect(() => {
    return () => {
      if (timerRef.current) {
        clearTimeout(timerRef.current);
      }
    };
  }, []);

  const debouncedCallback = useCallback(
    (...arguments_: Parameters<T>) => {
      if (timerRef.current) {
        clearTimeout(timerRef.current);
      }

      timerRef.current = setTimeout(() => {
        callback(...arguments_);
      }, delay);
    },
    [callback, delay]
  ) as T;

  return debouncedCallback;
};
