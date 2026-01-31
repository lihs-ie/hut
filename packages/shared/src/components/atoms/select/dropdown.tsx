"use client";

import { useState, useRef, useEffect, type ReactNode } from "react";
import styles from "./dropdown.module.css";
import { ChevronDownIcon } from "../icon/chevron-down";
import { CheckIcon } from "../icon/check";

export interface CustomSelectOption {
  value: string;
  label: string;
  icon?: ReactNode;
  description?: string;
}

export type Props = {
  value: string;
  onChange: (value: string) => void;
  options: CustomSelectOption[];
  className?: string;
  name: string;
};

export const DropdownSelect = ({
  value,
  onChange,
  options,
  className = "",
}: Props) => {
  const [isOpen, setIsOpen] = useState(false);
  const containerRef = useRef<HTMLDivElement>(null);

  const selectedOption = options.find((opt) => opt.value === value);

  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (
        containerRef.current &&
        !containerRef.current.contains(event.target as Node)
      ) {
        setIsOpen(false);
      }
    };

    document.addEventListener("mousedown", handleClickOutside);
    return () => document.removeEventListener("mousedown", handleClickOutside);
  }, []);

  const handleSelect = (optionValue: string) => {
    onChange(optionValue);
    setIsOpen(false);
  };

  return (
    <div ref={containerRef} className={`${styles.container} ${className}`}>
      <button
        type="button"
        className={styles.trigger}
        onClick={() => setIsOpen(!isOpen)}
        aria-expanded={isOpen}
      >
        <div className={styles.triggerContent}>
          {selectedOption?.icon && (
            <span className={styles.icon}>{selectedOption.icon}</span>
          )}
          <span className={styles.label}>{selectedOption?.label}</span>
        </div>
        <ChevronDownIcon
          className={`${styles.chevron} ${isOpen ? styles.chevronOpen : ""}`}
        />
      </button>

      {isOpen && (
        <div className={styles.dropdown}>
          {options.map((option) => (
            <button
              key={option.value}
              type="button"
              className={`${styles.option} ${
                value === option.value ? styles.optionSelected : ""
              }`}
              onClick={() => handleSelect(option.value)}
            >
              <div className={styles.optionContent}>
                {option.icon && (
                  <span className={styles.icon}>{option.icon}</span>
                )}
                <div className={styles.optionText}>
                  <div className={styles.optionLabel}>{option.label}</div>
                  {option.description && (
                    <div className={styles.optionDescription}>
                      {option.description}
                    </div>
                  )}
                </div>
              </div>
              {value === option.value && (
                <CheckIcon className={styles.checkIcon} />
              )}
            </button>
          ))}
        </div>
      )}
    </div>
  );
};
