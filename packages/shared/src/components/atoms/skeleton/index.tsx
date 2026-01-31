import styles from "./skeleton.module.css";

type Props = {
  width?: string | number;
  height?: string | number;
  variant?: "text" | "circle" | "card" | "rectangle";
  className?: string;
};

export const Skeleton = (props: Props) => {
  const variantClass = props.variant ? styles[props.variant] : "";

  const style: React.CSSProperties = {
    width: props.width,
    height: props.height,
  };

  return (
    <div
      className={`${styles.container} ${variantClass} ${props.className ?? ""}`}
      style={style}
    >
      <div className={styles.shimmer} />
    </div>
  );
};

type SkeletonTextProps = {
  lines?: number;
  width?: string | number;
  gap?: string | number;
  className?: string;
};

export const SkeletonText = (props: SkeletonTextProps) => {
  const lines = props.lines ?? 1;
  const gap = props.gap ?? "0.5rem";

  return (
    <div
      className={props.className}
      style={{ display: "flex", flexDirection: "column", gap }}
    >
      {Array.from({ length: lines }).map((_, index) => (
        <Skeleton
          key={index}
          variant="text"
          width={index === lines - 1 && lines > 1 ? "60%" : props.width}
        />
      ))}
    </div>
  );
};

type SkeletonCircleProps = {
  size?: string | number;
  className?: string;
};

export const SkeletonCircle = (props: SkeletonCircleProps) => {
  const size = props.size ?? "2.5rem";

  return (
    <Skeleton
      variant="circle"
      width={size}
      height={size}
      className={props.className}
    />
  );
};
