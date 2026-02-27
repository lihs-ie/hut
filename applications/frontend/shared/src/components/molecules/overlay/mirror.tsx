"use client";

import { useMemo } from "react";
import type { SpellCheckIssue } from "@shared/domains/spellcheck/common";
import styles from "./mirror.module.css";

type Props = {
  text: string;
  issues: SpellCheckIssue[];
  scrollTop: number;
  scrollLeft: number;
};

type Segment = {
  text: string;
  highlighted: boolean;
};

const buildSegments = (
  text: string,
  issues: SpellCheckIssue[],
): Segment[] => {
  if (issues.length === 0) {
    return [{ text, highlighted: false }];
  }

  const sorted = [...issues].sort((a, b) => a.offset - b.offset);
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
  const segments = useMemo(
    () => buildSegments(props.text, props.issues),
    [props.text, props.issues],
  );

  return (
    <div
      className={styles.container}
      style={{
        transform: `translate(${-props.scrollLeft}px, ${-props.scrollTop}px)`,
      }}
    >
      {segments.map((segment, index) =>
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
