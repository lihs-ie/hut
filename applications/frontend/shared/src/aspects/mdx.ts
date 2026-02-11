import { err, ok, Result } from "./result";

type Mattered<T> = {
  content: string;
  data: T;
};

export type MatterError = {
  message: string;
};

const parseYamlValue = (value: string): unknown => {
  const trimmed = value.trim();

  // 配列のパース: [item1, item2, ...]
  if (trimmed.startsWith("[") && trimmed.endsWith("]")) {
    const inner = trimmed.slice(1, -1).trim();
    if (inner === "") return [];
    return inner.split(",").map((item) => item.trim());
  }

  // 数値のパース
  if (/^-?\d+(\.\d+)?$/.test(trimmed)) {
    return Number(trimmed);
  }

  // booleanのパース
  if (trimmed === "true") return true;
  if (trimmed === "false") return false;

  // nullのパース
  if (trimmed === "null" || trimmed === "~") return null;

  return trimmed;
};

const defaultFrontMatter = <T extends Record<string, unknown>>(
  rawData: string
): T => {
  const lines = rawData.split("\n");
  const data: Record<string, unknown> = {};

  for (const line of lines) {
    const [key, ...rest] = line.split(":");
    const value = rest.join(":").trim();
    data[key.trim()] = parseYamlValue(value);
  }

  return data as T;
};

export const matter =
  <FM extends Record<string, unknown>>(
    frontMatter: (rawData: string) => FM = defaultFrontMatter
  ) =>
  <T>(transformer: (frontMatter: FM) => Result<T, MatterError>) =>
  (source: string): Result<Mattered<T>, MatterError> => {
    const match = /^---\n([\s\S]+?)\n---/.exec(source);

    if (!match) {
      return err({
        message: `Invalid front matter format: ${source}`,
      });
    }

    const rawData = match[1];
    const content = source.slice(match[0].length).trimStart();

    const frontMatterData = frontMatter(rawData);
    const transformed = transformer(frontMatterData);

    if (transformed.isErr) {
      return err({ message: transformed.unwrapError().message });
    }

    return ok({
      content,
      data: transformed.unwrap(),
    });
  };
