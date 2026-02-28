"use client";

import { useCallback, useMemo, useRef, useState } from "react";
import { MarkdownEditor } from "@shared/components/organisms/common/editor/markdown-editor";
import type { ImageUploadConfig } from "@shared/components/organisms/common/editor/markdown-editor";
import { TextMirror, buildSegments } from "@shared/components/molecules/overlay/mirror";
import { SuggestionTooltip } from "@shared/components/molecules/tooltip/suggestion";
import { useSpellcheck } from "@shared/components/global/hooks/use-spellcheck";
import type { ImageIdentifier } from "@shared/domains/image";
import type { SpellCheckIssue } from "@shared/components/global/workers/spellcheck-protocol";
import styles from "./spellcheck-editor.module.css";

type Props = {
  value: string;
  onChange: (value: string) => void;
  placeholder?: string;
  imageUpload?: ImageUploadConfig;
  onImageUploaded?: (imageIdentifier: ImageIdentifier, url: string) => void;
  onUploadingChange?: (uploading: boolean) => void;
  spellcheckEnabled: boolean;
};

type TooltipState = {
  issue: SpellCheckIssue;
  top: number;
  left: number;
} | null;

export const SpellcheckEditor = (props: Props) => {
  const textareaRef = useRef<HTMLTextAreaElement>(null);
  const [scrollTop, setScrollTop] = useState(0);
  const [scrollLeft, setScrollLeft] = useState(0);
  const [tooltip, setTooltip] = useState<TooltipState>(null);

  const { issues } = useSpellcheck(props.value, {
    enabled: props.spellcheckEnabled,
  });

  const mirrorSegments = useMemo(
    () => buildSegments(props.value, issues),
    [props.value, issues],
  );

  const handleScroll = useCallback(() => {
    const textarea = textareaRef.current;
    if (textarea) {
      setScrollTop(textarea.scrollTop);
      setScrollLeft(textarea.scrollLeft);
    }
  }, []);

  const handleClick = useCallback(
    (event: React.MouseEvent) => {
      const textarea = textareaRef.current;
      if (!textarea || !props.spellcheckEnabled || issues.length === 0) {
        setTooltip(null);
        return;
      }

      const cursorPosition = textarea.selectionStart;

      const matchingIssue = issues.find(
        (issue) =>
          cursorPosition >= issue.offset &&
          cursorPosition <= issue.offset + issue.length,
      );

      if (matchingIssue) {
        const rect = textarea.getBoundingClientRect();
        setTooltip({
          issue: matchingIssue,
          top: event.clientY - rect.top + 20,
          left: event.clientX - rect.left,
        });
      } else {
        setTooltip(null);
      }
    },
    [props.spellcheckEnabled, issues],
  );

  const currentValue = props.value;
  const onChange = props.onChange;

  const handleSuggestionSelect = useCallback(
    (suggestion: string) => {
      if (!tooltip) {
        return;
      }

      const before = currentValue.substring(0, tooltip.issue.offset);
      const after = currentValue.substring(
        tooltip.issue.offset + tooltip.issue.length,
      );
      onChange(before + suggestion + after);
      setTooltip(null);
    },
    [currentValue, onChange, tooltip],
  );

  const handleChange = useCallback(
    (value: string) => {
      setTooltip(null);
      onChange(value);
    },
    [onChange],
  );

  return (
    <div className={styles.container}>
      <div
        className={styles.wrapper}
        onClick={handleClick}
        onScroll={handleScroll}
      >
        {props.spellcheckEnabled && issues.length > 0 && (
          <TextMirror
            segments={mirrorSegments}
            scrollTop={scrollTop}
            scrollLeft={scrollLeft}
          />
        )}

        <MarkdownEditor
          value={props.value}
          onChange={handleChange}
          placeholder={props.placeholder}
          imageUpload={props.imageUpload}
          onImageUploaded={props.onImageUploaded}
          onUploadingChange={props.onUploadingChange}
          textareaRef={textareaRef}
        />

        {tooltip && (
          <SuggestionTooltip
            word={tooltip.issue.word}
            suggestions={tooltip.issue.suggestions}
            top={tooltip.top}
            left={tooltip.left}
            onSelect={handleSuggestionSelect}
            onDismiss={() => setTooltip(null)}
          />
        )}
      </div>
    </div>
  );
};
