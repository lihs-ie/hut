"use client";

import { useEffect, useRef } from "react";
import type { SearchReferenceIdentifier } from "@shared/domains/search-token/reference";

type Props = {
  identifier: SearchReferenceIdentifier;
  incrementViewCount: (identifier: SearchReferenceIdentifier) => Promise<void>;
};

export const ViewTracker = (props: Props) => {
  const hasTracked = useRef(false);
  const identifier = props.identifier;
  const incrementViewCount = props.incrementViewCount;

  useEffect(() => {
    if (hasTracked.current) {
      return;
    }
    hasTracked.current = true;
    incrementViewCount(identifier).catch(() => {});
  }, [incrementViewCount, identifier]);

  return null;
};
