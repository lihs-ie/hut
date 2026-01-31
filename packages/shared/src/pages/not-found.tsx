import { ErrorIndex } from "@shared/components/templates/error";

export default async function NotFoundPage() {
  return (
    <ErrorIndex
      code="404"
      title="ページが見つかりません"
      message="お探しのページは存在しないか、移動した可能性があります。"
      showHomeButton
    />
  );
}
