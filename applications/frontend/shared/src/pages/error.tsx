"use client";

import { ErrorIndex } from "@shared/components/templates/error";

export type Props = {
  error: Error & { digest?: string };
  reset: () => void;
};

export default function ErrorPage(props: Props) {
  return (
    <ErrorIndex
      code="500"
      title="エラーが発生しました"
      message={props.error.message || "予期しないエラーが発生しました。もう一度お試しください。"}
      showHomeButton
      onRetry={props.reset}
    />
  );
}
