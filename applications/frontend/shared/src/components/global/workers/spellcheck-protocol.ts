export const Severity = {
  ERROR: "error",
  WARNING: "warning",
  INFO: "info",
} as const;

export type Severity = (typeof Severity)[keyof typeof Severity];

export type SpellCheckIssue = {
  offset: number;
  length: number;
  word: string;
  suggestions: string[];
  severity: Severity;
  message: string;
};

export type SpellCheckRequest =
  | { type: "init"; dictionaryAff: string; dictionaryDic: string }
  | {
      type: "check";
      id: string;
      segments: Array<{ offset: number; text: string }>;
    };

export type SpellCheckResponse =
  | { type: "ready" }
  | { type: "result"; id: string; issues: SpellCheckIssue[] }
  | { type: "error"; message: string };
