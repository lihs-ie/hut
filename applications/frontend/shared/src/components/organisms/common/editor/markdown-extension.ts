export const parseMessageBox = (input: string): string => {
  return input.replace(
    /:::message( alert)?\n([\s\S]*?):::/g,
    (_match, alertSuffix: string | undefined, content: string) => {
      const className = alertSuffix ? "message alert" : "message";
      return `<div class="${className}">${content.trim()}</div>`;
    },
  );
};

export const parseAccordion = (input: string): string => {
  return input.replace(
    /:{3,}details (.+)\n([\s\S]*?):{3,}/g,
    (_match, title: string, content: string) => {
      return `<details>\n<summary>${title.trim()}</summary>\n${content.trim()}\n</details>`;
    },
  );
};

export const parseCodeBlockFilename = (input: string): string => {
  return input.replace(
    /```(\w+):(\S+)\n([\s\S]*?)```/g,
    (_match, language: string, filename: string, code: string) => {
      return `<div class="code-block-with-filename"><div class="code-block-filename">${filename}</div>\n\`\`\`${language}\n${code}\`\`\`</div>`;
    },
  );
};

export const parseImageWidth = (input: string): string => {
  return input.replace(
    /!\[([^\]]*)\]\(([^)]+) =(\d+)x\)/g,
    (_match, alt: string, src: string, width: string) => {
      return `<img src="${src}" alt="${alt}" width="${width}" />`;
    },
  );
};
