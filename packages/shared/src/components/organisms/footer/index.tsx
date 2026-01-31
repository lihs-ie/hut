import Link from "next/link";
import {
  GithubIcon,
  MailIcon,
  XTwitterIcon,
} from "@shared/components/atoms/icon";
import styles from "./index.module.css";
import { ExternalServiceType } from "@shared/domains/common/service";
import { Routes } from "@shared/config/presentation/route";

export type Props = {
  externalServices: Map<ExternalServiceType, string>;
};

export const Footer = async () => (
  <footer className={styles.container}>
    <div className={styles.contents}>
      <div className={styles.grid}>
        <div className={styles.section}>
          <h3 className={styles.sectionTitle}>About</h3>
          <ul className={styles.linkList}>
            <li>
              <Link href="/about">プロフィール</Link>
            </li>
            <li>
              <Link href="/about#experience">経歴</Link>
            </li>
            <li>
              <Link href="/about#tech-stack">技術スタック</Link>
            </li>
          </ul>
        </div>

        <div className={styles.section}>
          <h3 className={styles.sectionTitle}>Content</h3>
          <ul className={styles.linkList}>
            <li>
              <Link href={Routes.page.articles.index}>記事</Link>
            </li>
            <li>
              <Link href={Routes.page.memos.index}>メモ</Link>
            </li>
            {/* [初期リリース対象外]
            <li>
              <Link href={Routes.page.series.index}>シリーズ</Link>
            </li>
            */}
          </ul>
        </div>

        <div className={styles.section}>
          <h3 className={styles.sectionTitle}>Links</h3>
          <ul className={styles.linkList}>
            <li>
              <Link href="/search">検索</Link>
            </li>
          </ul>
        </div>

        <div className={styles.section}>
          <h3 className={styles.sectionTitle}>Legal</h3>
          <ul className={styles.linkList}>
            <li>
              <Link href="/privacy">プライバシーポリシー</Link>
            </li>
          </ul>
        </div>
      </div>

      <div className={styles.bottom}>
        <div className={styles.social}>
          <a
            href="https://github.com"
            target="_blank"
            rel="noopener noreferrer"
            aria-label="GitHub"
          >
            <GithubIcon className={styles["social-icon"]} />
          </a>
          <a
            href="https://twitter.com"
            target="_blank"
            rel="noopener noreferrer"
            aria-label="Twitter"
          >
            <XTwitterIcon className={styles["social-icon"]} />
          </a>
          <a href="mailto:contact@example.com" aria-label="Email">
            <MailIcon className={styles["social-icon"]} />
          </a>
        </div>
        <p className={styles.copyright}>© 2026 lihs. All rights reserved.</p>
      </div>
    </div>
  </footer>
);
