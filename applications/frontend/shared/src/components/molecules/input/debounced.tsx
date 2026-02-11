"use client";

import { useState } from "react";
import { useDebouncedCallback } from "@shared/components/global/hooks/use-debounce";
import { TextInput } from "../../atoms/input/text";
import styles from "./debounced.module.css";

const DEFAULT_DELAY = 300;

export type Props = {
  defaultValue?: string;
  onDebouncedChange: (value: string) => void;
  delay?: number;
  type?: "text" | "search";
  placeholder?: string;
  className?: string;
  autoFocus?: boolean;
  disabled?: boolean;
};

export const DebouncedTextInput = (props: Props) => {
  const [value, setValue] = useState(props.defaultValue ?? "");
  const debouncedOnChange = useDebouncedCallback(
    props.onDebouncedChange,
    props.delay ?? DEFAULT_DELAY
  );

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const newValue = event.target.value;
    setValue(newValue);
    debouncedOnChange(newValue);
  };

  return (
    <TextInput
      type={props.type}
      value={value}
      onChange={handleChange}
      placeholder={props.placeholder}
      className={`${styles.container} ${props.className ?? ""}`}
      autoFocus={props.autoFocus}
      disabled={props.disabled}
    />
  );
};
