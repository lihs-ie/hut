import { NavigableLink } from "@shared/components/molecules/link/navigable";

import styles from "./index.module.css";
import { ExternalServiceType } from "@shared/domains/common/service";
import { Routes } from "@shared/config/presentation/route";
import { MailAddress } from "@shared/domains/user";
import { GithubIcon } from "@shared/components/atoms/icon/github";
import { XTwitterIcon } from "@shared/components/atoms/icon/x-twitter";
import { MailIcon } from "@shared/components/atoms/icon/mail";

export type Props = {
  mailAddress: MailAddress | null;
  externalServices: Map<ExternalServiceType, string>;
};

export const FooterPresenter = (props: Props) => (
  <footer className={styles.container}>
    <div className={styles.contents}>
      <div className={styles.grid}>
        <div className={styles.section}>
          <h3 className={styles.sectionTitle}>About</h3>
          <ul className={styles.linkList}>
            <li>
              <NavigableLink href="/about">プロフィール</NavigableLink>
            </li>
            <li>
              <NavigableLink href="/about#experience">経歴</NavigableLink>
            </li>
            <li>
              <NavigableLink href="/about#tech-stack">技術スタック</NavigableLink>
            </li>
          </ul>
        </div>

        <div className={styles.section}>
          <h3 className={styles.sectionTitle}>Content</h3>
          <ul className={styles.linkList}>
            <li>
              <NavigableLink href={Routes.page.articles.index}>記事</NavigableLink>
            </li>
            <li>
              <NavigableLink href={Routes.page.memos.index}>メモ</NavigableLink>
            </li>
            <li>
              <NavigableLink href={Routes.page.series.index}>連載</NavigableLink>
            </li>
          </ul>
        </div>

        <div className={styles.section}>
          <h3 className={styles.sectionTitle}>Links</h3>
          <ul className={styles.linkList}>
            <li>
              <NavigableLink href="/search">検索</NavigableLink>
            </li>
          </ul>
        </div>

        <div className={styles.section}>
          <h3 className={styles.sectionTitle}>Legal</h3>
          <ul className={styles.linkList}>
            <li>
              <NavigableLink href="/privacy">プライバシーポリシー</NavigableLink>
            </li>
          </ul>
        </div>
      </div>

      <div className={styles.bottom}>
        <div className={styles.social}>
          {props.externalServices.has(ExternalServiceType.GITHUB) && (
            <a
              href={`https://github.com/${props.externalServices.get(ExternalServiceType.GITHUB)}`}
              target="_blank"
              rel="noopener noreferrer"
              aria-label="GitHub"
            >
              <span className={styles.icon}>
                <GithubIcon className={styles["social-icon"]} />
              </span>
            </a>
          )}

          {props.externalServices.has(ExternalServiceType.X) && (
            <a
              href={`https://x.com/${props.externalServices.get(ExternalServiceType.X)}`}
              target="_blank"
              rel="noopener noreferrer"
              aria-label="Twitter"
            >
              <span className={styles.icon}>
                <XTwitterIcon className={styles["social-icon"]} />
              </span>
            </a>
          )}

          {props.mailAddress !== null && (
            <a href={`mailto:${props.mailAddress}`} aria-label="Email">
              <MailIcon className={styles["social-icon"]} />
            </a>
          )}
        </div>
        <p className={styles.copyright}>© 2026 lihs. All rights reserved.</p>
      </div>
    </div>
  </footer>
);
