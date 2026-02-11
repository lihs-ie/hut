"use client";

import { ReactNode, useEffect, useRef, useState } from "react";
import { DropDownButton } from "./button";
import { DropDownContent } from "./content";
import styles from "./index.module.css";

export type Props = {
  children: ReactNode;
  content: ReactNode;
};

export const DropDown = (props: Props) => {
  const [open, setOpen] = useState(false);
  const [dropdoenTop, setDropdownTop] = useState<number | null>(null);

  const dropDownRef = useRef<HTMLDivElement>(null);
  const buttonRef = useRef<HTMLDivElement>(null);
  const contentRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const handler = (event: MouseEvent) => {
      if (
        dropDownRef.current &&
        !dropDownRef.current.contains(event.target as Node)
      ) {
        setOpen(false);
      }
    };

    document.addEventListener("click", handler);

    return () => {
      document.removeEventListener("click", handler);
    };
  }, [dropDownRef]);

  const toggle = () => {
    if (!open) {
      const spaceRemaining =
        window.innerHeight - buttonRef.current!.getBoundingClientRect().bottom;

      const contentHeight = contentRef.current!.clientHeight;

      const topPosition =
        spaceRemaining > contentHeight ? null : spaceRemaining - contentHeight;

      setDropdownTop(topPosition);
    }
    setOpen(!open);
  };

  return (
    <div className={styles.container} ref={dropDownRef}>
      <DropDownButton ref={buttonRef} toggle={toggle}>
        {props.children}
      </DropDownButton>
      <DropDownContent ref={contentRef} open={open} top={dropdoenTop}>
        {props.content}
      </DropDownContent>
    </div>
  );
};
