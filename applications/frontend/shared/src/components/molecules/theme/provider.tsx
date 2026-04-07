"use client";

import { ThemeProvider as NextThemesProvider } from "next-themes";
import React from "react";

type Props = {
  children: React.ReactNode;
};

export const ThemeProvider = (props: Props) => (
  <NextThemesProvider
    attribute="class"
    defaultTheme="light"
    themes={["light", "dark"]}
    disableTransitionOnChange
  >
    {props.children}
  </NextThemesProvider>
);
