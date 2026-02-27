import type {
  SpellCheckRequest,
  SpellCheckResponse,
  SpellCheckIssue,
} from "@shared/domains/spellcheck/common";

type NSpell = {
  correct: (word: string) => boolean;
  suggest: (word: string) => string[];
};

let spellChecker: NSpell | null = null;

const WORD_PATTERN = /[a-zA-Z']+/g;

const checkSegments = (
  segments: Array<{ offset: number; text: string }>,
): SpellCheckIssue[] => {
  if (!spellChecker) {
    return [];
  }

  const issues: SpellCheckIssue[] = [];

  for (const segment of segments) {
    let match;
    WORD_PATTERN.lastIndex = 0;

    while ((match = WORD_PATTERN.exec(segment.text)) !== null) {
      const word = match[0];
      if (word.length < 2) {
        continue;
      }

      if (!spellChecker.correct(word)) {
        issues.push({
          offset: segment.offset + match.index,
          length: word.length,
          word,
          suggestions: spellChecker.suggest(word).slice(0, 5),
          severity: "error",
          message: `"${word}" may be misspelled`,
        });
      }
    }
  }

  return issues;
};

const postResponse = (response: SpellCheckResponse) => {
  self.postMessage(response);
};

const extractErrorMessage = (error: unknown, fallback: string): string =>
  error instanceof Error ? error.message : fallback;

const handleInit = async (request: Extract<SpellCheckRequest, { type: "init" }>) => {
  try {
    const nspellModule = await import("nspell");
    const createSpellChecker = nspellModule.default ?? nspellModule;
    spellChecker = createSpellChecker(
      request.dictionaryAff,
      request.dictionaryDic,
    );
    postResponse({ type: "ready" });
  } catch (error) {
    postResponse({
      type: "error",
      message: extractErrorMessage(error, "Failed to initialize spell checker"),
    });
  }
};

const handleCheck = (request: Extract<SpellCheckRequest, { type: "check" }>) => {
  try {
    const issues = checkSegments(request.segments);
    postResponse({ type: "result", id: request.id, issues });
  } catch (error) {
    postResponse({
      type: "error",
      message: extractErrorMessage(error, "Spell check failed"),
    });
  }
};

self.addEventListener("message", async (event: MessageEvent<SpellCheckRequest>) => {
  const request = event.data;

  switch (request.type) {
    case "init":
      await handleInit(request);
      break;
    case "check":
      handleCheck(request);
      break;
  }
});
