"use client";

import {
  createContext,
  useContext,
  useState,
  useCallback,
  useEffect,
  useRef,
} from "react";
import { usePathname } from "next/navigation";
import { TopProgressBar } from "@shared/components/molecules/navigation/progress-bar";

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
  showBar: boolean;
  completing: boolean;
};

export const NavigationProvider = (props: Props) => {
  const pathname = usePathname();
  const [state, setState] = useState<NavigationState>({
    trackedPathname: pathname,
    isNavigating: false,
    showBar: false,
    completing: false,
  });
  const timerRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const completingTimerRef = useRef<ReturnType<typeof setTimeout> | null>(
    null,
  );

  if (state.trackedPathname !== pathname) {
    if (state.showBar) {
      setState({
        trackedPathname: pathname,
        isNavigating: false,
        showBar: true,
        completing: true,
      });
    } else {
      setState({
        trackedPathname: pathname,
        isNavigating: false,
        showBar: false,
        completing: false,
      });
    }
  }

  useEffect(() => {
    if (!state.completing) {
      return;
    }
    completingTimerRef.current = setTimeout(() => {
      setState((previous) => ({
        ...previous,
        showBar: false,
        completing: false,
      }));
    }, 200);
    return () => {
      if (completingTimerRef.current !== null) {
        clearTimeout(completingTimerRef.current);
      }
    };
  }, [state.completing]);

  useEffect(() => {
    return () => {
      if (timerRef.current !== null) {
        clearTimeout(timerRef.current);
      }
    };
  }, [pathname]);

  const startNavigation = useCallback(() => {
    setState((previous) => ({
      ...previous,
      isNavigating: true,
      completing: false,
    }));
    if (timerRef.current !== null) {
      clearTimeout(timerRef.current);
    }
    timerRef.current = setTimeout(() => {
      setState((previous) => {
        if (!previous.isNavigating) {
          return previous;
        }
        return { ...previous, showBar: true };
      });
    }, 200);
  }, []);

  return (
    <NavigationContext.Provider
      value={{ isNavigating: state.isNavigating, startNavigation }}
    >
      {props.children}
      <TopProgressBar visible={state.showBar} completing={state.completing} />
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
