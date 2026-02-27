import { SaveIcon } from "@shared/components/atoms/icon/save";
import { SimpleSwitch } from "@shared/components/atoms/toggle/simple";
import { SpellcheckToggle } from "@shared/components/molecules/toggle/spellcheck";
import styles from "./header.module.css";
import { useRouter } from "next/navigation";
import { BackButton } from "@shared/components/molecules/button/back";

type Props = {
  title: string;
  onTitleChange: (title: string) => void;
  isPublished: boolean;
  onPublishChange: (value: boolean) => void;
  persist: () => Promise<void>;
  isLoading?: boolean;
  isUploading?: boolean;
  spellcheckEnabled?: boolean;
  onSpellcheckChange?: (enabled: boolean) => void;
};

export const EditorHeader = (props: Props) => {
  const router = useRouter();

  return (
    <header className={styles.container}>
      <div className={styles.inner}>
        <div className={styles["left-section"]}>
          <BackButton onClick={() => router.back()} />
          <input
            type="text"
            value={props.title}
            onChange={(event) => props.onTitleChange(event.target.value)}
            placeholder={"タイトルを入力"}
            className={styles["title-input"]}
          />
        </div>

        <div className={styles["right-section"]}>
          {props.onSpellcheckChange && (
            <SpellcheckToggle
              checked={props.spellcheckEnabled ?? false}
              onChange={props.onSpellcheckChange}
            />
          )}

          <div className={styles["publish-toggle"]}>
            <span className={styles["publish-label"]}>公開</span>
            <SimpleSwitch
              checked={props.isPublished}
              onChange={props.onPublishChange}
              aria-label="公開"
            />
          </div>

          <button
            type="button"
            onClick={() => props.persist()}
            disabled={!props.title.trim() || props.isUploading}
            className={styles["save-button"]}
          >
            <SaveIcon className={styles.icon} />
            {props.isUploading
              ? "画像アップロード中..."
              : props.isLoading
                ? "保存中..."
                : props.isPublished
                  ? "公開する"
                  : "下書き保存"}
          </button>
        </div>
      </div>
    </header>
  );
};
