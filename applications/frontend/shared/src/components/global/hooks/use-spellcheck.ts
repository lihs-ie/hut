"use client";

import { useEffect, useMemo, useRef, useState } from "react";
import { useDebounce } from "@shared/components/global/hooks/use-debounce";
import { extractCheckableSegments } from "@shared/domains/common/markdown";
import type {
  SpellCheckIssue,
  SpellCheckResponse,
} from "@shared/domains/spellcheck/common";

type UseSpellcheckOptions = {
  enabled: boolean;
  debounceDelay?: number;
};

type UseSpellcheckReturn = {
  issues: SpellCheckIssue[];
  isReady: boolean;
};

const DEFAULT_DEBOUNCE_DELAY = 500;
const EMPTY_ISSUES: SpellCheckIssue[] = [];

type WorkerState = {
  issues: SpellCheckIssue[];
  isReady: boolean;
};

export const useSpellcheck = (
  text: string,
  options: UseSpellcheckOptions,
): UseSpellcheckReturn => {
  const [workerState, setWorkerState] = useState<WorkerState>({
    issues: [],
    isReady: false,
  });
  const workerRef = useRef<Worker | null>(null);
  const checkIdRef = useRef(0);

  const debouncedText = useDebounce(
    text,
    options.debounceDelay ?? DEFAULT_DEBOUNCE_DELAY,
  );

  const segments = useMemo(
    () => extractCheckableSegments(debouncedText),
    [debouncedText],
  );

  const hasCheckableContent = segments.length > 0;

  useEffect(() => {
    if (!options.enabled) {
      if (workerRef.current) {
        workerRef.current.terminate();
        workerRef.current = null;
      }
      return;
    }

    const worker = new Worker(
      new URL("../workers/spellcheck.worker.ts", import.meta.url),
      { type: "module" },
    );

    worker.addEventListener(
      "message",
      (event: MessageEvent<SpellCheckResponse>) => {
        const response = event.data;

        switch (response.type) {
          case "ready":
            setWorkerState((previous) => ({ ...previous, isReady: true }));
            break;
          case "result":
            setWorkerState((previous) => ({
              ...previous,
              issues: response.issues,
            }));
            break;
          case "error":
            break;
        }
      },
    );

    workerRef.current = worker;

    const initializeWorker = async () => {
      try {
        const [affResponse, dicResponse] = await Promise.all([
          fetch("/dictionaries/en.aff"),
          fetch("/dictionaries/en.dic"),
        ]);

        if (!affResponse.ok || !dicResponse.ok) {
          return;
        }

        const [dictionaryAff, dictionaryDic] = await Promise.all([
          affResponse.text(),
          dicResponse.text(),
        ]);

        worker.postMessage({
          type: "init",
          dictionaryAff,
          dictionaryDic,
        });
      } catch {
        void 0;
      }
    };

    initializeWorker();

    return () => {
      worker.terminate();
      workerRef.current = null;
      setWorkerState({ issues: [], isReady: false });
    };
  }, [options.enabled]);

  useEffect(() => {
    if (!options.enabled || !workerState.isReady || !workerRef.current || !hasCheckableContent) {
      return;
    }

    checkIdRef.current += 1;
    const currentId = String(checkIdRef.current);

    workerRef.current.postMessage({
      type: "check",
      id: currentId,
      segments,
    });
  }, [segments, options.enabled, workerState.isReady, hasCheckableContent]);

  const isReady = options.enabled && workerState.isReady;
  const issues = useMemo(() => {
    if (!isReady || !hasCheckableContent) {
      return EMPTY_ISSUES;
    }
    return workerState.issues;
  }, [isReady, hasCheckableContent, workerState.issues]);

  return { issues, isReady };
};
