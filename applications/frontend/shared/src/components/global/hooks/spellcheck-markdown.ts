type MarkdownToken = {
  type: "text" | "syntax" | "code" | "frontmatter";
  offset: number;
  length: number;
  content: string;
};

const createToken = (
  type: MarkdownToken["type"],
  offset: number,
  content: string,
): MarkdownToken => ({
  type,
  offset,
  length: content.length,
  content,
});

const tokenizeWrappedSyntax = (
  wrapper: string,
  innerContent: string,
  offset: number,
): MarkdownToken[] => [
  createToken("syntax", offset, wrapper),
  createToken("text", offset + wrapper.length, innerContent),
  createToken(
    "syntax",
    offset + wrapper.length + innerContent.length,
    wrapper,
  ),
];

type InlinePattern = {
  regex: RegExp;
  tokenize: (match: RegExpMatchArray, offset: number) => MarkdownToken[];
  advance: (match: RegExpMatchArray) => number;
};

const INLINE_PATTERNS: InlinePattern[] = [
  {
    regex: /^`([^`]+)`/,
    tokenize: (match, offset) => [createToken("code", offset, match[0])],
    advance: (match) => match[0].length,
  },
  {
    regex: /^<!--[\s\S]*?-->/,
    tokenize: (match, offset) => [createToken("syntax", offset, match[0])],
    advance: (match) => match[0].length,
  },
  {
    regex: /^!\[[^\]]*\]\([^)]*\)/,
    tokenize: (match, offset) => [createToken("syntax", offset, match[0])],
    advance: (match) => match[0].length,
  },
  {
    regex: /^\[([^\]]*)\]\(([^)]*)\)/,
    tokenize: (match, offset) => {
      const linkText = match[1];
      const linkUrl = match[2];
      let position = offset;
      const tokens: MarkdownToken[] = [];
      tokens.push(createToken("syntax", position, "["));
      position += 1;
      tokens.push(createToken("text", position, linkText));
      position += linkText.length;
      tokens.push(createToken("syntax", position, "]("));
      position += 2;
      tokens.push(createToken("syntax", position, linkUrl));
      position += linkUrl.length;
      tokens.push(createToken("syntax", position, ")"));
      return tokens;
    },
    advance: (match) => match[0].length,
  },
  {
    regex: /^\*\*(.+?)\*\*/,
    tokenize: (match, offset) => tokenizeWrappedSyntax("**", match[1], offset),
    advance: (match) => match[0].length,
  },
  {
    regex: /^__(.+?)__/,
    tokenize: (match, offset) => tokenizeWrappedSyntax("__", match[1], offset),
    advance: (match) => match[0].length,
  },
  {
    regex: /^~~(.+?)~~/,
    tokenize: (match, offset) => tokenizeWrappedSyntax("~~", match[1], offset),
    advance: (match) => match[0].length,
  },
  {
    regex: /^\*(.+?)\*/,
    tokenize: (match, offset) => tokenizeWrappedSyntax("*", match[1], offset),
    advance: (match) => match[0].length,
  },
];

const SPECIAL_MARKERS = ["`", "<!--", "![", "[", "**", "__", "~~", "*"];

const tokenizeLine = (
  line: string,
  lineOffset: number,
): MarkdownToken[] => {
  const tokens: MarkdownToken[] = [];
  let position = 0;

  const headingMatch = line.match(/^(#{1,6} )/);
  if (headingMatch) {
    tokens.push(createToken("syntax", lineOffset, headingMatch[1]));
    position = headingMatch[1].length;
  }

  while (position < line.length) {
    const remaining = line.substring(position);
    const currentOffset = lineOffset + position;

    let matched = false;
    for (const pattern of INLINE_PATTERNS) {
      const match = remaining.match(pattern.regex);
      if (match) {
        tokens.push(...pattern.tokenize(match, currentOffset));
        position += pattern.advance(match);
        matched = true;
        break;
      }
    }
    if (matched) {
      continue;
    }

    let nextSpecial = line.length;
    for (const marker of SPECIAL_MARKERS) {
      const index = line.indexOf(marker, position);
      if (index !== -1 && index < nextSpecial && index > position) {
        nextSpecial = index;
      }
    }

    const textContent = line.substring(position, nextSpecial);
    if (textContent.length > 0) {
      tokens.push(createToken("text", currentOffset, textContent));
    }
    position = nextSpecial;
  }

  return tokens;
};

export const tokenizeMarkdown = (text: string): MarkdownToken[] => {
  if (text.length === 0) {
    return [];
  }

  const tokens: MarkdownToken[] = [];
  let position = 0;

  const frontmatterMatch = text.match(/^---\n[\s\S]*?\n---/);
  if (frontmatterMatch) {
    tokens.push(
      createToken("frontmatter", 0, frontmatterMatch[0]),
    );
    position = frontmatterMatch[0].length;
  }

  while (position < text.length) {
    const remaining = text.substring(position);

    const codeBlockMatch = remaining.match(/^```[^\n]*\n[\s\S]*?\n```/);
    if (codeBlockMatch) {
      tokens.push(
        createToken("code", position, codeBlockMatch[0]),
      );
      position += codeBlockMatch[0].length;
      continue;
    }

    const lineEnd = text.indexOf("\n", position);
    const line =
      lineEnd === -1
        ? text.substring(position)
        : text.substring(position, lineEnd);

    const lineTokens = tokenizeLine(line, position);
    tokens.push(...lineTokens);

    if (lineEnd !== -1) {
      tokens.push(createToken("text", lineEnd, "\n"));
      position = lineEnd + 1;
    } else {
      position = text.length;
    }
  }

  return tokens;
};

export const extractCheckableSegments = (
  text: string,
): Array<{ offset: number; text: string }> => {
  const tokens = tokenizeMarkdown(text);
  return tokens
    .filter((token) => token.type === "text")
    .map((token) => ({ offset: token.offset, text: token.content }));
};
