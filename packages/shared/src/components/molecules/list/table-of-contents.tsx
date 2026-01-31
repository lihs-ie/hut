"use client";

import { useState } from "react";
import { ChevronDownIcon } from "@shared/components/atoms/icon";
import styles from "./table-of-contents.module.css";
import { Node } from "@shared/components/global/mdx";

export type Props = {
  root: Node[];
};

type TocNodeProps = {
  node: Node;
  onMobileClick?: () => void;
};

const TocNodeDesktop = ({ node }: TocNodeProps) => (
  <>
    <a
      key={node.identifier}
      href={`#${node.identifier}`}
      className={styles.link}
    >
      {node.headline}
    </a>
    {node.children.length > 0 && (
      <div className={styles.nested}>
        {node.children.map((child) => (
          <TocNodeDesktop key={child.identifier} node={child} />
        ))}
      </div>
    )}
  </>
);

const TocNodeMobile = ({ node, onMobileClick }: TocNodeProps) => (
  <>
    <a
      key={node.identifier}
      href={`#${node.identifier}`}
      className={styles.mobileLink}
      onClick={onMobileClick}
    >
      {node.headline}
    </a>
    {node.children.length > 0 && (
      <div className={styles.nested}>
        {node.children.map((child) => (
          <TocNodeMobile
            key={child.identifier}
            node={child}
            onMobileClick={onMobileClick}
          />
        ))}
      </div>
    )}
  </>
);

export const TableOfContents = (props: Props) => {
  const [isOpen, setIsOpen] = useState(false);
  const handleMobileClick = () => setIsOpen(false);

  return (
    <>
      <div className={styles.desktopToc}>
        <h3 className={styles.title}>目次</h3>
        <nav className={styles.nav}>
          {props.root.map((node) => (
            <TocNodeDesktop key={node.identifier} node={node} />
          ))}
        </nav>
      </div>

      <div className={`${styles.mobileToc} ${isOpen ? styles.open : ""}`}>
        <button
          className={styles.toggleButton}
          onClick={() => setIsOpen(!isOpen)}
          aria-expanded={isOpen}
        >
          <span className={styles.toggleText}>目次</span>
          <ChevronDownIcon
            className={`${styles.toggleIcon} ${isOpen ? styles.rotated : ""}`}
          />
        </button>

        {isOpen && (
          <nav className={styles.mobileNav}>
            {props.root.map((node) => (
              <TocNodeMobile
                key={node.identifier}
                node={node}
                onMobileClick={handleMobileClick}
              />
            ))}
          </nav>
        )}
      </div>
    </>
  );
};
