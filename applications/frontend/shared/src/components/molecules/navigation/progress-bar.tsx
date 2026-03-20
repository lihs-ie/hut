import styles from "./progress-bar.module.css";

type Props = {
  visible: boolean;
  completing?: boolean;
};

export const TopProgressBar = (props: Props) => {
  if (!props.visible) {
    return null;
  }

  return (
    <div
      role="progressbar"
      aria-label="Loading"
      data-testid="top-progress-bar"
      data-completing={String(props.completing ?? false)}
      className={props.completing ? styles.completing : styles.bar}
    />
  );
};
