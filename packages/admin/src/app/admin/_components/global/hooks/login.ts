"use client";

import { useCallback, useEffect, useState } from "react";
import { useRouter } from "next/navigation";
import { OIDCClientProvider } from "@/providers/acl/oidc/client";
import { login } from "@/actions/auth";
import { fromPromise, ok } from "@shared/aspects/result";
import { useServerAction } from "@shared/components/global/hooks/use-server-action";

export type LoginState = "idle" | "checking" | "loading" | "error";

export type UseLoginOptions = {
  redirectPath?: string;
};

export type UseLoginResult = {
  state: LoginState;
  errorMessage: string | null;
  handleGoogleLogin: () => Promise<void>;
};

type RedirectOutcome = "idle" | "loggedIn";

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
  const [state, setState] = useState<LoginState>("checking");
  const [errorMessage, setErrorMessage] = useState<string | null>(null);
  const { execute: executeLogin } = useServerAction(login);
  const redirectPath = options.redirectPath ?? "/";

  const handleRedirectResult = useCallback(async (): Promise<void> => {
    await OIDCClientProvider.getRedirectResult()
      .mapError((error) => resolveErrorMessage(error, redirectErrorMessage))
      .andThen((user) => {
        if (user === null) {
          return ok<RedirectOutcome, string>("idle").toAsync();
        }

        setState("loading");

        return OIDCClientProvider.getIdToken()
          .mapError((error) => resolveErrorMessage(error, redirectErrorMessage))
          .andThen((idToken) =>
            fromPromise(executeLogin(idToken), (error) =>
              resolveErrorMessage(error, redirectErrorMessage),
            ),
          )
          .map((): RedirectOutcome => "loggedIn");
      })
      .match({
        ok: (outcome) => {
          if (outcome === "idle") {
            setState("idle");
            return;
          }

          router.push(redirectPath);
        },
        err: (message) => {
          setState("error");
          setErrorMessage(message);
        },
      });
  }, [executeLogin, redirectPath, router]);

  useEffect(() => {
    void handleRedirectResult();
  }, [handleRedirectResult]);

  const handleGoogleLogin = useCallback(async (): Promise<void> => {
    setState("loading");
    setErrorMessage(null);

    await OIDCClientProvider.startRedirect()
      .mapError((error) =>
        resolveErrorMessage(error, startRedirectErrorMessage),
      )
      .match({
        ok: () => undefined,
        err: (message) => {
          setState("error");
          setErrorMessage(message);
        },
      });
  }, []);

  return { state, errorMessage, handleGoogleLogin };
};
