type Props = {
  html: string;
  fallback: string;
};

const escapeHtml = (text: string): string =>
  text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;");

export const MermaidSvg = (props: Props) => {
  const isFallback = props.fallback === "true";

  if (isFallback) {
    return (
      <pre className="mermaid-svg fallback">
        <code
          dangerouslySetInnerHTML={{
            __html: escapeHtml(props.html),
          }}
        />
      </pre>
    );
  }

  return (
    <div
      className="mermaid-svg"
      dangerouslySetInnerHTML={{ __html: props.html }}
    />
  );
};

export default MermaidSvg;
