import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { useState } from "react";
import { ErrorModal } from "@shared/components/molecules/modal/error";

const meta = {
  component: ErrorModal,
  parameters: {
    layout: "fullscreen",
  },
  argTypes: {
    isOpen: {
      control: "boolean",
      description: "モーダルの表示状態",
    },
    title: {
      control: "text",
      description: "モーダルのタイトル",
    },
    message: {
      control: "text",
      description: "エラーメッセージ",
    },
    closeText: {
      control: "text",
      description: "閉じるボタンのテキスト",
    },
  },
} satisfies Meta<typeof ErrorModal>;

export default meta;

type Story = StoryObj<typeof ErrorModal>;

export const Default: Story = {
  args: {
    isOpen: true,
    message: "データの保存中にエラーが発生しました。",
  },
  render: (args) => {
    const [isOpen, setIsOpen] = useState(args.isOpen);

    return (
      <>
        <button onClick={() => setIsOpen(true)}>エラーモーダルを開く</button>
        <ErrorModal {...args} isOpen={isOpen} onClose={() => setIsOpen(false)} />
      </>
    );
  },
};

export const WithCustomTitle: Story = {
  args: {
    isOpen: true,
    title: "接続エラー",
    message: "サーバーとの接続に失敗しました。ネットワーク接続を確認してください。",
  },
  render: (args) => {
    const [isOpen, setIsOpen] = useState(args.isOpen);

    return (
      <>
        <button onClick={() => setIsOpen(true)}>エラーモーダルを開く</button>
        <ErrorModal {...args} isOpen={isOpen} onClose={() => setIsOpen(false)} />
      </>
    );
  },
};

export const WithDetails: Story = {
  args: {
    isOpen: true,
    title: "入力エラー",
    message: "入力内容に問題があります。以下を確認してください。",
    details: [
      { field: "email", description: "メールアドレスの形式が正しくありません" },
      { field: "password", description: "パスワードは8文字以上で入力してください" },
      { field: "username", description: "ユーザー名に使用できない文字が含まれています" },
    ],
  },
  render: (args) => {
    const [isOpen, setIsOpen] = useState(args.isOpen);

    return (
      <>
        <button onClick={() => setIsOpen(true)}>エラーモーダルを開く</button>
        <ErrorModal {...args} isOpen={isOpen} onClose={() => setIsOpen(false)} />
      </>
    );
  },
};

export const WithDetailsNoField: Story = {
  args: {
    isOpen: true,
    message: "以下のエラーが発生しました。",
    details: [
      { description: "認証トークンの有効期限が切れています" },
      { description: "セッションを再度開始してください" },
    ],
  },
  render: (args) => {
    const [isOpen, setIsOpen] = useState(args.isOpen);

    return (
      <>
        <button onClick={() => setIsOpen(true)}>エラーモーダルを開く</button>
        <ErrorModal {...args} isOpen={isOpen} onClose={() => setIsOpen(false)} />
      </>
    );
  },
};

export const WithCustomCloseText: Story = {
  args: {
    isOpen: true,
    message: "操作を完了できませんでした。",
    closeText: "OK",
  },
  render: (args) => {
    const [isOpen, setIsOpen] = useState(args.isOpen);

    return (
      <>
        <button onClick={() => setIsOpen(true)}>エラーモーダルを開く</button>
        <ErrorModal {...args} isOpen={isOpen} onClose={() => setIsOpen(false)} />
      </>
    );
  },
};

export const Closed: Story = {
  args: {
    isOpen: false,
    message: "このメッセージは表示されません。",
  },
  render: (args) => {
    const [isOpen, setIsOpen] = useState(args.isOpen);

    return (
      <>
        <button onClick={() => setIsOpen(true)}>エラーモーダルを開く</button>
        <ErrorModal {...args} isOpen={isOpen} onClose={() => setIsOpen(false)} />
      </>
    );
  },
};
