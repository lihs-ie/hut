import type { SpellCheckIssue } from "@shared/components/global/workers/spellcheck-protocol";
import styles from "./mirror.module.css";

type Segment = {
  text: string;
  highlighted: boolean;
};

type Props = {
  segments: Segment[];
  scrollTop: number;
  scrollLeft: number;
};

export type { Segment };

export const buildSegments = (
  text: string,
  issues: SpellCheckIssue[],
): Segment[] => {
  if (issues.length === 0) {
    return [{ text, highlighted: false }];
  }

  const sorted = issues.toSorted((a, b) => a.offset - b.offset);
  const segments: Segment[] = [];
  let position = 0;

  for (const issue of sorted) {
    if (issue.offset > position) {
      segments.push({
        text: text.substring(position, issue.offset),
        highlighted: false,
      });
    }

    segments.push({
      text: text.substring(issue.offset, issue.offset + issue.length),
      highlighted: true,
    });

    position = issue.offset + issue.length;
  }

  if (position < text.length) {
    segments.push({
      text: text.substring(position),
      highlighted: false,
    });
  }

  return segments;
};

export const TextMirror = (props: Props) => {
  return (
    <div
      className={styles.container}
      style={{
        transform: `translate(${-props.scrollLeft}px, ${-props.scrollTop}px)`,
      }}
    >
      {props.segments.map((segment, index) =>
        segment.highlighted ? (
          <mark key={index} className={styles.highlight}>
            {segment.text}
          </mark>
        ) : (
          <span key={index}>{segment.text}</span>
        ),
      )}
    </div>
  );
};
