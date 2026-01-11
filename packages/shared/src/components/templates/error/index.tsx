"use client";

import Link from "next/link";
import { ErrorMessage } from "@shared/components/atoms/text/error";
import styles from "./index.module.css";
import { VariantButton } from "@shared/components/atoms/button/variant";
import { useRouter } from "next/navigation";
import { useTransition } from "react";
import { DotsSpinner } from "@shared/components/molecules/spinner/dots";

type Props = {
  code: string;
  title: string;
  message: string;
  showHomeButton?: boolean;
  showBackButton?: boolean;
  onRetry?: () => void;
};

export const ErrorIndex = (props: Props) => {
  const router = useRouter();
  const [isPending, startTransition] = useTransition();

  const handleRetry: () => void = () => {
    startTransition(() => {
      if (props.onRetry) {
        props.onRetry();
      } else {
        router.refresh();
      }
    });
  };

  if (isPending) {
    return (
      <div className={styles.container}>
        <div className={styles.loading}>
          <div className={styles.spinner}>
            <DotsSpinner />
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className={styles.container}>
      <ErrorMessage
        code={props.code}
        title={props.title}
        message={props.message}
      />
      <div className={styles.actions}>
        <VariantButton className={styles.retry} onClick={handleRetry}>
          もう一度試す
        </VariantButton>
        {props.showHomeButton && (
          <Link href="/" className={styles.top}>
            <VariantButton variant="outline" className={styles.top}>
              トップページへ
            </VariantButton>
          </Link>
        )}
      </div>
    </div>
  );
};
