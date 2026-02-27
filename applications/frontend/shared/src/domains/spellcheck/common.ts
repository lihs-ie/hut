import z from "zod";

export const Severity = {
  ERROR: "error",
  WARNING: "warning",
  INFO: "info",
} as const;

export type Severity = (typeof Severity)[keyof typeof Severity];

export const severitySchema = z.enum(["error", "warning", "info"]);

export const spellCheckIssueSchema = z.object({
  offset: z.number().nonnegative(),
  length: z.number().positive(),
  word: z.string().min(1),
  suggestions: z.array(z.string()),
  severity: severitySchema,
  message: z.string(),
});

export type SpellCheckIssue = z.infer<typeof spellCheckIssueSchema>;

export const spellCheckResultSchema = z.object({
  text: z.string(),
  issues: z.array(spellCheckIssueSchema),
  timestamp: z.number(),
});

export type SpellCheckResult = z.infer<typeof spellCheckResultSchema>;

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
