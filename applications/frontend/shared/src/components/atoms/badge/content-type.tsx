import { forwardRef, type HTMLAttributes } from "react";
import styles from "./content-type.module.css";

export type Props = HTMLAttributes<HTMLSpanElement> & {
  variant?: "default" | "secondary" | "outline";
};

export const ContentTypeBadge = forwardRef<HTMLSpanElement, Props>(
  (props: Props, ref) => {
    const { variant = "default", className, ...rest } = props;

    return (
      <span
        ref={ref}
        className={`${styles.container} ${styles[variant]} ${className || ""}`}
        {...rest}
      />
    );
  }
);

ContentTypeBadge.displayName = "ContentTypeBadge";
