import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { PrivacySectionsPresenter } from "@shared/components/organisms/common/document/privacy.presenter";
import { Forger } from "@lihs-ie/forger-ts";
import {
  PrivacyPolicyMold,
  PrivacyPolicySectionMold,
} from "../../../../../support/molds/domains/document";

const meta = {
  component: PrivacySectionsPresenter,
} satisfies Meta<typeof PrivacySectionsPresenter>;

export default meta;

export const Default: StoryObj<typeof PrivacySectionsPresenter> = {
  args: {
    privacy: Forger(PrivacyPolicyMold).forge(),
  },
};

export const ManySections: StoryObj<typeof PrivacySectionsPresenter> = {
  args: {
    privacy: Forger(PrivacyPolicyMold).forge({
      sections: Forger(PrivacyPolicySectionMold).forgeMulti(6),
    }),
  },
};

export const WithoutLists: StoryObj<typeof PrivacySectionsPresenter> = {
  args: {
    privacy: Forger(PrivacyPolicyMold).forge({
      sections: Forger(PrivacyPolicySectionMold).forgeMulti(2, { list: null }),
    }),
  },
};
