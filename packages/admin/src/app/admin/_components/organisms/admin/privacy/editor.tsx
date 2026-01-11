import {
  PrivacyPolicy,
  UnvalidatedPrivacyPolicy,
} from "@shared/domains/document";
import { PolicyEditorPresenter } from "./editor.presenter";

export type Props = {
  persist: (unvalidated: UnvalidatedPrivacyPolicy) => Promise<void>;
  getPrivacyPolicy: () => Promise<PrivacyPolicy>;
};

export const PolicyEditorForm = async (props: Props) => {
  const privacyPolicy = await props.getPrivacyPolicy();

  return (
    <PolicyEditorPresenter persist={props.persist} value={privacyPolicy} />
  );
};
