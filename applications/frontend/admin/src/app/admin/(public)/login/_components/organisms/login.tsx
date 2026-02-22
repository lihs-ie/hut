"use client";

import { useLogin } from "@/app/admin/_components/global/hooks/login";
import { SimpleCard } from "@shared/components/atoms/card/simple";
import styles from "./login.module.css";
import { LockIcon } from "@shared/components/atoms/icon/lock";
import { GoogleLoginButton } from "@/app/admin/_components/atoms/button/google-login";

export type Props = {
  redirectURI: string;
};

export const Login = (props: Props) => {
  const { state, errorMessage, handleGoogleLogin } = useLogin({
    redirectPath: props.redirectURI,
  });

  return (
    <div className={styles.container}>
      <SimpleCard className={styles.card}>
        <div className={styles.header}>
          <div className={styles.icon}>
            <LockIcon />
          </div>
          <h1 className={styles.title}>管理者ログイン</h1>
          <p className={styles.description}>
            Google アカウントでログインしてください
          </p>
        </div>

        {errorMessage !== null ? (
          <div className={styles.error}>{errorMessage}</div>
        ) : null}

        <div className={styles.button}>
          <GoogleLoginButton
            login={handleGoogleLogin}
            disabled={state === "loading"}
          />
        </div>

        <p className={styles.hint}>
          許可された Google アカウントのみログインできます
        </p>
      </SimpleCard>
    </div>
  );
};
