"use client";

import { useEffect, useRef, useState } from "react";
import "./svg.css";

type Props = {
  code: string;
};

export const MermaidClient = (props: Props) => {
  const container = useRef<HTMLDivElement>(null);
  const [error, setError] = useState<string | null>(null);
  const [rendered, setRendered] = useState(false);

  useEffect(() => {
    setError(null);
    setRendered(false);

    const target = container.current;
    if (target === null) return;

    let cancelled = false;

    const render = async () => {
      try {
        const mermaid = (await import("mermaid")).default;
        mermaid.initialize({
          startOnLoad: false,
          theme: "default",
          securityLevel: "strict",
        });
        const id = `mermaid-${Math.random().toString(36).slice(2)}`;
        const { svg } = await mermaid.render(id, props.code);
        if (cancelled) return;
        target.innerHTML = svg;
        setRendered(true);
      } catch (cause) {
        if (cancelled) return;
        setError(cause instanceof Error ? cause.message : String(cause));
      }
    };

    render();

    return () => {
      cancelled = true;
    };
  }, [props.code]);

  if (error !== null) {
    return (
      <pre className="mermaid-svg fallback">
        <code>{props.code}</code>
      </pre>
    );
  }

  return (
    <div ref={container} className="mermaid-svg">
      {!rendered && (
        <pre className="mermaid-svg fallback">
          <code>{props.code}</code>
        </pre>
      )}
    </div>
  );
};

export default MermaidClient;
