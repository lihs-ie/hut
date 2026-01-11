"use client";

import { useState, useCallback } from "react";
import type { ErrorDetail } from "@shared/components/molecules/modal/error";

export type ServerActionError = {
  message: string;
  details?: ErrorDetail[];
};

type ServerActionState<T> = {
  data: T | null;
  error: ServerActionError | null;
  isLoading: boolean;
  isError: boolean;
  isSuccess: boolean;
};

type UseServerActionReturn<
  T,
  TArgs extends unknown[],
> = ServerActionState<T> & {
  execute: (...arguments_: TArgs) => Promise<T>;
  clearError: () => void;
  reset: () => void;
};

const parseError = (error: unknown): ServerActionError => {
  if (error instanceof Error) {
    const errorWithDetails = error as Error & {
      errors?: Array<{ field?: string; description?: string }>;
    };

    return {
      message: error.message,
      details: errorWithDetails.errors?.map((error) => ({
        field: error.field,
        description: error.description,
      })),
    };
  }

  if (typeof error === "string") {
    return { message: error };
  }

  return { message: "予期しないエラーが発生しました" };
};

export const useServerAction = <T, TArgs extends unknown[]>(
  action: (...arguments_: TArgs) => Promise<T>,
): UseServerActionReturn<T, TArgs> => {
  const [state, setState] = useState<ServerActionState<T>>({
    data: null,
    error: null,
    isLoading: false,
    isError: false,
    isSuccess: false,
  });

  const execute = useCallback(
    async (...arguments_: TArgs): Promise<T> => {
      setState({
        data: null,
        error: null,
        isLoading: true,
        isError: false,
        isSuccess: false,
      });

      try {
        const result = await action(...arguments_);
        setState({
          data: result,
          error: null,
          isLoading: false,
          isError: false,
          isSuccess: true,
        });
        return result;
      } catch (error) {
        const parsedError = parseError(error);
        setState({
          data: null,
          error: parsedError,
          isLoading: false,
          isError: true,
          isSuccess: false,
        });

        return Promise.reject(parsedError);
      }
    },
    [action],
  );

  const clearError = useCallback(() => {
    setState((previous) => ({
      ...previous,
      error: null,
      isError: false,
    }));
  }, []);

  const reset = useCallback(() => {
    setState({
      data: null,
      error: null,
      isLoading: false,
      isError: false,
      isSuccess: false,
    });
  }, []);

  return {
    ...state,
    execute,
    clearError,
    reset,
  };
};
