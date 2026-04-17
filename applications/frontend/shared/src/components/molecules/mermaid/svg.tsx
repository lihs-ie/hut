type Props = {
  html: string;
  fallback?: boolean;
};

export const MermaidSvg = (props: Props) => {
  if (props.fallback) {
    return (
      <pre className="mermaid-svg fallback">
        <code>{props.html}</code>
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
