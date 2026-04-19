"use client";

import { useEffect, useId, useState } from "react";
import { useTheme } from "next-themes";
import "./svg.css";

type Props = {
  code: string;
};

export const MermaidClient = (props: Props) => {
  const { resolvedTheme } = useTheme();
  const reactId = useId();
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
        // securityLevel: "strict" は mermaid 内蔵サニタイザ。 on*/foreignObject などの
        // 危険な要素を除去するので、ここで返る svg は dangerouslySetInnerHTML に
        // 渡しても XSS にならない (詳細: https://mermaid.js.org/config/usage.html#securitylevel)。
        mermaid.initialize({
          startOnLoad: false,
          theme: resolvedTheme === "dark" ? "dark" : "default",
          securityLevel: "strict",
        });
        const id = `mermaid-${reactId.replace(/:/g, "")}`;
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
  }, [props.code, resolvedTheme, reactId]);

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
