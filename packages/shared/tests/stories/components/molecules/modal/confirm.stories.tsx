import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";

import { ConfirmModal } from "@shared/components/molecules/modal/confirm";

const meta = {
  component: ConfirmModal,
} satisfies Meta<typeof ConfirmModal>;

export default meta;

export const Info: StoryObj<typeof ConfirmModal> = {
  args: {
    isOpen: true,
    title: "確認",
    message: "この操作を続行しますか？",
    type: "info",
    onClose: () => {},
    onConfirm: () => {},
  },
};

export const Warning: StoryObj<typeof ConfirmModal> = {
  args: {
    isOpen: true,
    title: "警告",
    message: "この操作は取り消せません。続行しますか？",
    type: "warning",
    onClose: () => {},
    onConfirm: () => {},
  },
};

export const Success: StoryObj<typeof ConfirmModal> = {
  args: {
    isOpen: true,
    title: "成功",
    message: "操作が正常に完了しました。",
    type: "success",
    confirmText: "OK",
    onClose: () => {},
    onConfirm: () => {},
  },
};

export const Error: StoryObj<typeof ConfirmModal> = {
  args: {
    isOpen: true,
    title: "エラー",
    message: "操作中にエラーが発生しました。再試行しますか？",
    type: "error",
    confirmText: "再試行",
    cancelText: "閉じる",
    onClose: () => {},
    onConfirm: () => {},
  },
};

export const Interactive: StoryObj<typeof ConfirmModal> = {
  render: () => {
    const InteractiveModal = () => {
      const [isOpen, setIsOpen] = useState(false);
      return (
        <div>
          <button onClick={() => setIsOpen(true)}>モーダルを開く</button>
          <ConfirmModal
            isOpen={isOpen}
            title="確認"
            message="本当にこの操作を行いますか？"
            type="warning"
            onClose={() => setIsOpen(false)}
            onConfirm={() => {
              console.log("確認されました");
              setIsOpen(false);
            }}
          />
        </div>
      );
    };
    return <InteractiveModal />;
  },
};
