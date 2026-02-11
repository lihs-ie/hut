import { Decorator } from "@storybook/nextjs-vite";
import { NuqsTestingAdapter } from "nuqs/adapters/testing";

export const nuqsDecorator: Decorator = (Story) => {
  return (
    <NuqsTestingAdapter>
      <Story />
    </NuqsTestingAdapter>
  );
};
