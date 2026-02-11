export const Theme = {
  LIGHT: "light",
  DARK: "dark",
} as const;

export type Theme = (typeof Theme)[keyof typeof Theme];
