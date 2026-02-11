"use client";

import { useEffect, useRef, useCallback } from "react";
import type { ContentType } from "@shared/domains/search-token/reference";

type Props = {
  contentType: ContentType;
  contentIdentifier: string;
  contentElementId: string;
};

export const EngagementTracker = (props: Props) => {
  const activeTimeRef = useRef<number>(0);
  const lastActiveRef = useRef<number>(0);
  const maxScrollDepthRef = useRef<number>(0);
  const sentRef = useRef<boolean>(false);

  const sendEngagement = useCallback(() => {
    if (sentRef.current) return;
    sentRef.current = true;

    const data = {
      contentType: props.contentType,
      contentIdentifier: props.contentIdentifier,
      dwellTime: Math.round(activeTimeRef.current / 1000),
      maxScrollDepth: maxScrollDepthRef.current,
    };

    navigator.sendBeacon(
      "/api/engagement",
      new Blob([JSON.stringify(data)], { type: "application/json" }),
    );
  }, [props.contentType, props.contentIdentifier]);

  useEffect(() => {
    lastActiveRef.current = Date.now();

    const handleVisibilityChange = () => {
      if (document.hidden) {
        activeTimeRef.current += Date.now() - lastActiveRef.current;
      } else {
        lastActiveRef.current = Date.now();
      }
    };

    const contentElement = document.getElementById(props.contentElementId);
    if (!contentElement) return;

    const observer = new IntersectionObserver(
      (entries) => {
        for (const entry of entries) {
          if (entry.isIntersecting) {
            const depth = Number(entry.target.getAttribute("data-depth"));
            if (depth > maxScrollDepthRef.current) {
              maxScrollDepthRef.current = depth;
            }
          }
        }
      },
      { threshold: 0.5 },
    );

    const contentHeight = contentElement.scrollHeight;
    const markers: HTMLDivElement[] = [];

    [25, 50, 75, 100].forEach((depth) => {
      const marker = document.createElement("div");
      marker.setAttribute("data-depth", String(depth));
      marker.style.position = "absolute";
      marker.style.top = `${(contentHeight * depth) / 100}px`;
      marker.style.height = "1px";
      marker.style.width = "1px";
      contentElement.style.position = "relative";
      contentElement.appendChild(marker);
      observer.observe(marker);
      markers.push(marker);
    });

    document.addEventListener("visibilitychange", handleVisibilityChange);
    window.addEventListener("beforeunload", sendEngagement);

    return () => {
      document.removeEventListener("visibilitychange", handleVisibilityChange);
      window.removeEventListener("beforeunload", sendEngagement);
      observer.disconnect();

      for (const marker of markers) {
        if (marker.parentNode) {
          marker.parentNode.removeChild(marker);
        }
      }

      sendEngagement();
    };
  }, [props.contentElementId, sendEngagement]);

  return null;
};
