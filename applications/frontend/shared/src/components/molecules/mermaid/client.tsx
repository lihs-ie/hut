"use client";

import { useEffect, useState } from "react";
import { useTheme } from "next-themes";
import "./svg.css";

type Props = {
  code: string;
};

export const MermaidClient = (props: Props) => {
  const { resolvedTheme } = useTheme();
  const [svg, setSvg] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    setError(null);
    setSvg(null);

    if (resolvedTheme === undefined) {
      return;
    }

    let cancelled = false;

    const render = async () => {
      try {
        const mermaid = (await import("mermaid")).default;
        mermaid.initialize({
          startOnLoad: false,
          theme: resolvedTheme === "dark" ? "dark" : "default",
          securityLevel: "strict",
        });
        const id = `mermaid-${Math.random().toString(36).slice(2)}`;
        const { svg: rendered } = await mermaid.render(id, props.code);
        if (cancelled) return;
        setSvg(rendered);
      } catch (cause) {
        if (cancelled) return;
        setError(cause instanceof Error ? cause.message : String(cause));
      }
    };

    render();

    return () => {
      cancelled = true;
    };
  }, [props.code, resolvedTheme]);

  if (error !== null) {
    return (
      <pre className="mermaid-svg fallback">
        <code>{props.code}</code>
      </pre>
    );
  }

  if (svg !== null) {
    return (
      <div
        className="mermaid-svg"
        dangerouslySetInnerHTML={{ __html: svg }}
      />
    );
  }

  return (
    <pre className="mermaid-svg fallback">
      <code>{props.code}</code>
    </pre>
  );
};

export default MermaidClient;
