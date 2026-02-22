"use client";

import { useCallback, useState } from "react";
import { useRouter } from "next/navigation";
import { OIDCClientProvider } from "@/providers/acl/oidc/client";
import { login } from "@/actions/auth";
import { fromPromise } from "@shared/aspects/result";
import { useServerAction } from "@shared/components/global/hooks/use-server-action";

export type LoginState = "idle" | "loading" | "error";

export type UseLoginOptions = {
  redirectPath?: string;
};

export type UseLoginResult = {
  state: LoginState;
  errorMessage: string | null;
  handleGoogleLogin: () => Promise<void>;
};

const redirectErrorMessage = "認証処理中にエラーが発生しました";
const startRedirectErrorMessage = "ログイン処理を開始できませんでした";

const resolveErrorMessage = (error: unknown, fallback: string): string => {
  if (typeof error === "object" && error !== null && "message" in error) {
    const message = (error as { message?: unknown }).message;
    if (typeof message === "string") {
      return message;
    }
  }

  return fallback;
};

export const useLogin = (options: UseLoginOptions = {}): UseLoginResult => {
  const router = useRouter();
  const [state, setState] = useState<LoginState>("idle");
  const [errorMessage, setErrorMessage] = useState<string | null>(null);
  const { execute: executeLogin } = useServerAction(login);
  const redirectPath = options.redirectPath ?? "/";

  const handleGoogleLogin = useCallback(async (): Promise<void> => {
    setState("loading");
    setErrorMessage(null);

    await OIDCClientProvider.startPopup()
      .mapError((error) => resolveErrorMessage(error, startRedirectErrorMessage))
      .andThen(() =>
        OIDCClientProvider.getIdToken().mapError((error) =>
          resolveErrorMessage(error, redirectErrorMessage),
        ),
      )
      .andThen((idToken) =>
        fromPromise(executeLogin(idToken), (error) =>
          resolveErrorMessage(error, redirectErrorMessage),
        ),
      )
      .match({
        ok: () => {
          router.push(redirectPath);
        },
        err: (message) => {
          setState("error");
          setErrorMessage(message);
        },
      });
  }, [executeLogin, redirectPath, router]);

  return { state, errorMessage, handleGoogleLogin };
};
