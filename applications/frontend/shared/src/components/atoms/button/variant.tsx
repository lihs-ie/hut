import { ReactNode } from "react";
import styles from "./variant.module.css";

export type Props = {
  variant?: "default" | "outline" | "ghost";
  size?: "sm" | "md";
  onClick?: () => void;
  disabled?: boolean;
  children: ReactNode;
  className?: string;
  type?: HTMLButtonElement["type"];
};

export const VariantButton = (props: Props) => {
  const variantClass = styles[props.variant || "default"];
  const sizeClass = styles[props.size || "md"];

  return (
    <button
      className={`${styles.container} ${variantClass} ${sizeClass} ${props.className || ""}`}
      onClick={props.onClick}
      disabled={props.disabled}
      type={props.type}
    >
      {props.children}
    </button>
  );
};
