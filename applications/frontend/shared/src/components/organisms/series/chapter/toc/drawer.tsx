"use client";

import {
  MouseEvent,
  ReactNode,
  useEffect,
  useState,
  useSyncExternalStore,
} from "react";

import { MenuIcon } from "@shared/components/atoms/icon/hamburger";
import { XIcon } from "@shared/components/atoms/icon/cross";

import styles from "./drawer.module.css";

export type Props = {
  children: ReactNode;
};

const MOBILE_MEDIA_QUERY = "(max-width: 1023px)";

const subscribeToMobileMediaQuery = (callback: () => void) => {
  const mediaQueryList = window.matchMedia(MOBILE_MEDIA_QUERY);
  mediaQueryList.addEventListener("change", callback);
  return () => mediaQueryList.removeEventListener("change", callback);
};

const getMobileMediaQuerySnapshot = () =>
  window.matchMedia(MOBILE_MEDIA_QUERY).matches;

const getMobileMediaQueryServerSnapshot = () => false;

export const TOCDrawer = (props: Props) => {
  const [isOpen, setIsOpen] = useState(false);
  const isMobile = useSyncExternalStore(
    subscribeToMobileMediaQuery,
    getMobileMediaQuerySnapshot,
    getMobileMediaQueryServerSnapshot,
  );

  useEffect(() => {
    if (!isOpen) {
      return;
    }

    const handleEscape = (event: KeyboardEvent) => {
      if (event.key === "Escape") {
        setIsOpen(false);
      }
    };

    document.addEventListener("keydown", handleEscape);
    document.body.style.overflow = "hidden";

    return () => {
      document.removeEventListener("keydown", handleEscape);
      document.body.style.overflow = "unset";
    };
  }, [isOpen]);

  const handleDrawerClick = (event: MouseEvent<HTMLDivElement>) => {
    if ((event.target as HTMLElement).closest("a")) {
      setIsOpen(false);
    }
  };

  const isInactive = isMobile && !isOpen;

  return (
    <div className={styles.container}>
      <button
        type="button"
        className={styles.trigger}
        onClick={() => setIsOpen(true)}
        aria-label="目次を開く"
        aria-expanded={isOpen}
        aria-controls="chapter-toc-drawer"
      >
        <MenuIcon className={styles.icon} />
      </button>
      {isOpen && (
        <div
          className={styles.overlay}
          onClick={() => setIsOpen(false)}
          aria-hidden="true"
        />
      )}
      <div
        id="chapter-toc-drawer"
        className={styles.drawer}
        data-open={isOpen}
        inert={isInactive}
        aria-hidden={isInactive}
        onClick={handleDrawerClick}
      >
        <button
          type="button"
          className={styles.close}
          onClick={() => setIsOpen(false)}
          aria-label="目次を閉じる"
        >
          <XIcon className={styles.icon} />
        </button>
        {props.children}
      </div>
    </div>
  );
};
