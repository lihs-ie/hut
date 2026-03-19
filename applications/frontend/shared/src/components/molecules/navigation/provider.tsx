"use client";

import { createContext, useContext, useState, useCallback } from "react";
import { usePathname } from "next/navigation";
import { LoadingOverlay } from "@shared/components/molecules/overlay/loading";

type NavigationContextValue = {
  isNavigating: boolean;
  startNavigation: () => void;
};

const NavigationContext = createContext<NavigationContextValue | null>(null);

type Props = {
  children: React.ReactNode;
};

type NavigationState = {
  trackedPathname: string;
  isNavigating: boolean;
};

export const NavigationProvider = (props: Props) => {
  const pathname = usePathname();
  const [state, setState] = useState<NavigationState>({
    trackedPathname: pathname,
    isNavigating: false,
  });

  if (state.trackedPathname !== pathname) {
    setState({ trackedPathname: pathname, isNavigating: false });
  }

  const startNavigation = useCallback(() => {
    setState((previous) => ({ ...previous, isNavigating: true }));
  }, []);

  return (
    <NavigationContext.Provider value={{ isNavigating: state.isNavigating, startNavigation }}>
      {props.children}
      {state.isNavigating && <LoadingOverlay />}
    </NavigationContext.Provider>
  );
};

export const useNavigation = (): NavigationContextValue => {
  const context = useContext(NavigationContext);
  if (!context) {
    throw new Error(
      "useNavigation は NavigationProvider の配下で使用してください",
    );
  }
  return context;
};
