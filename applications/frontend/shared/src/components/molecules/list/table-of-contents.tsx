"use client";

import { useState } from "react";
import { ChevronDownIcon } from "@shared/components/atoms/icon/chevron-down";
import styles from "./table-of-contents.module.css";
import { Node } from "@shared/components/global/mdx";

export type Props = {
  root: Node[];
};

type TOCNodeProps = {
  node: Node;
  onMobileClick?: () => void;
};

const TOCNodeDesktop = ({ node }: TOCNodeProps) => (
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
          <TOCNodeDesktop key={child.identifier} node={child} />
        ))}
      </div>
    )}
  </>
);

const TOCNodeMobile = ({ node, onMobileClick }: TOCNodeProps) => (
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
          <TOCNodeMobile
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
            <TOCNodeDesktop key={node.identifier} node={node} />
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
          <div className={styles.icon}>
            <ChevronDownIcon
              className={`${styles.toggleIcon} ${isOpen ? styles.rotated : ""}`}
              />
          </div>
        </button>

        {isOpen && (
          <nav className={styles.mobileNav}>
            {props.root.map((node) => (
              <TOCNodeMobile
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
