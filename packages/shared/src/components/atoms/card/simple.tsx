import { forwardRef, type HTMLAttributes } from "react";
import styles from "./simple.module.css";

export type Props = HTMLAttributes<HTMLDivElement>;

export const SimpleCard = forwardRef<HTMLDivElement, Props>(
  ({ className, ...props }, ref) => {
    return (
      <div
        ref={ref}
        className={`${styles.container} ${className || ""}`}
        {...props}
      />
    );
  }
);

SimpleCard.displayName = "SimpleCard";
