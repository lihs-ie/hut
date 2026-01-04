export const Theme = {
  LIGHT: "light",
  DARK: "dark",
  // SYSTEM: "system",　TODO: later
} as const;

export type Theme = (typeof Theme)[keyof typeof Theme];

export type Props = {
  value: Theme;
  onToggle: () => void;
};

export const ThemeToggle = (props: Props) => (
  <button onClick={props.onToggle} aria-label="Toggle Theme">
    {props.value === Theme.LIGHT ? "🌞" : "🌜"}
  </button>
);
