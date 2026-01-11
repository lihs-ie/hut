import type { Preview, Decorator } from "@storybook/nextjs-vite";
import "@shared/global.css";
import { nuqsDecorator } from "./decorator/nuqs";

const withTheme: Decorator = (Story, context) => {
  const theme = context.globals.theme;

  if (typeof document !== "undefined") {
    if (theme === "dark") {
      document.documentElement.classList.add("dark");
    } else {
      document.documentElement.classList.remove("dark");
    }
  }

  return Story();
};

const preview: Preview = {
  globalTypes: {
    theme: {
      description: "テーマを切り替え",
      toolbar: {
        title: "Theme",
        icon: "circlehollow",
        items: [
          { value: "light", icon: "sun", title: "Light" },
          { value: "dark", icon: "moon", title: "Dark" },
        ],
        dynamicTitle: true,
      },
    },
  },
  initialGlobals: {
    theme: "light",
  },
  decorators: [withTheme, nuqsDecorator],
  parameters: {
    controls: {
      matchers: {
        color: /(background|color)$/i,
        date: /Date$/i,
      },
    },
  },
};

export default preview;
