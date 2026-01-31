import { PolicyEditorForm } from "../../../organisms/admin/privacy/editor";
import styles from "./edit.module.css";
import {
  PrivacyPolicy,
  UnvalidatedPrivacyPolicy,
} from "@shared/domains/document";

export type Props = {
  getPrivacyPolicy: () => Promise<PrivacyPolicy>;
  persist: (unvalidated: UnvalidatedPrivacyPolicy) => Promise<void>;
};

export const PrivacyEdit = (props: Props) => (
  <div className={styles.container}>
    <PolicyEditorForm
      getPrivacyPolicy={props.getPrivacyPolicy}
      persist={props.persist}
    />
  </div>
);
