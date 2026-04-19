"use client";

import { ThemeProvider as NextThemesProvider } from "next-themes";
import React from "react";

type Props = {
  children: React.ReactNode;
  defaultTheme?: "light" | "dark" | "system";
};

export const ThemeProvider = (props: Props) => (
  <NextThemesProvider
    attribute="class"
    defaultTheme={props.defaultTheme ?? "light"}
    themes={["light", "dark"]}
    disableTransitionOnChange
  >
    {props.children}
  </NextThemesProvider>
);
