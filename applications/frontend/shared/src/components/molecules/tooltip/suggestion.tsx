"use client";

import styles from "./suggestion.module.css";

type Props = {
  word: string;
  suggestions: string[];
  top: number;
  left: number;
  onSelect: (suggestion: string) => void;
  onDismiss: () => void;
};

export const SuggestionTooltip = (props: Props) => {
  return (
    <div
      className={styles.container}
      style={{ top: `${props.top}px`, left: `${props.left}px` }}
      onMouseDown={(event) => event.preventDefault()}
    >
      <div className={styles.header}>{props.word}</div>
      {props.suggestions.length > 0 ? (
        <ul className={styles.list}>
          {props.suggestions.map((suggestion) => (
            <li
              key={suggestion}
              className={styles.item}
              onClick={() => props.onSelect(suggestion)}
            >
              {suggestion}
            </li>
          ))}
        </ul>
      ) : (
        <div className={styles.empty}>候補なし</div>
      )}
    </div>
  );
};
