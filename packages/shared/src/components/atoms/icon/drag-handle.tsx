import styles from "./drag-handle.module.css";

export type Props = {
  className?: string;
};

export const DragHandleIcon = (props: Props) => {
  return (
    <div className={`${styles.container} ${props.className || ""}`}>
      <span className={styles.dots}>⋮⋮</span>
    </div>
  );
};
