"use client";

import { useState, useCallback, useRef, useEffect } from "react";
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

type UseServerActionOptions<T> = {
  onSuccess?: (data: T) => void;
};

type UseServerActionReturn<
  T,
  TArgs extends unknown[],
> = ServerActionState<T> & {
  execute: (...arguments_: TArgs) => Promise<T>;
  clearError: () => void;
  reset: () => void;
};

type ErrorWithDetails = Error & {
  errors: Array<{ field?: string; description?: string }>;
};

const hasErrors = (error: Error): error is ErrorWithDetails =>
  "errors" in error && Array.isArray(error.errors);

const parseError = (error: unknown): ServerActionError => {
  if (error instanceof Error) {
    return {
      message: error.message,
      details: hasErrors(error)
        ? error.errors.map((detail) => ({
            field: detail.field,
            description: detail.description,
          }))
        : undefined,
    };
  }

  if (typeof error === "string") {
    return { message: error };
  }

  return { message: "予期しないエラーが発生しました" };
};

export const useServerAction = <T, TArgs extends unknown[]>(
  action: (...arguments_: TArgs) => Promise<T>,
  options?: UseServerActionOptions<T>,
): UseServerActionReturn<T, TArgs> => {
  const [state, setState] = useState<ServerActionState<T>>({
    data: null,
    error: null,
    isLoading: false,
    isError: false,
    isSuccess: false,
  });

  const onSuccessRef = useRef(options?.onSuccess);
  useEffect(() => {
    onSuccessRef.current = options?.onSuccess;
  });

  const actionRef = useRef(action);
  useEffect(() => {
    actionRef.current = action;
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
        const result = await actionRef.current(...arguments_);
        setState({
          data: result,
          error: null,
          isLoading: false,
          isError: false,
          isSuccess: true,
        });
        onSuccessRef.current?.(result);
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
    [],
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
