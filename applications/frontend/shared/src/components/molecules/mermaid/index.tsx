"use client";

import { useEffect, useRef, useState } from "react";
import styles from "./index.module.css";
import { sanitizeMermaidSvg } from "./sanitize";

type Props = {
  code: string;
};

export const MermaidRenderer = (props: Props) => {
  const containerRef = useRef<HTMLDivElement>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (!containerRef.current || !props.code) return;

    const container = containerRef.current;

    import("mermaid").then((module) => {
      const mermaid = module.default;
      mermaid.initialize({ startOnLoad: false, theme: "default" });

      const mermaidId = `mermaid-${Math.random().toString(36).slice(2)}`;

      mermaid
        .render(mermaidId, props.code)
        .then(({ svg }) => {
          container.innerHTML = sanitizeMermaidSvg(svg);
          setError(null);
        })
        .catch(() => {
          document.getElementById(mermaidId)?.remove();
          document.getElementById(`d${mermaidId}`)?.remove();
          setError(props.code);
        });
    });
  }, [props.code]);

  if (error) {
    return (
      <pre className={styles.fallback}>
        <code>{error}</code>
      </pre>
    );
  }

  return <div ref={containerRef} className={styles.container} />;
};
