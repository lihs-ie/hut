"use client";

import { fetchOGP, OGPMetadata } from "@shared/actions/ogp";
import styles from "./link.module.css";
import Image from "next/image";
import Link from "next/link";
import useSWR from "swr";

type Props = {
  url: string;
};

const getDomain = (url: string): string => {
  try {
    return new URL(url).hostname;
  } catch {
    return url;
  }
};

const LinkCardSkeleton = (props: { url: string }) => {
  const domain = getDomain(props.url);

  return (
    <div className={`${styles.container} ${styles.loading}`}>
      <div className={styles.card}>
        <div className={styles.content}>
          <div className={styles.text}>
            <div className={`${styles.title} ${styles.skeleton}`} />
            <div className={`${styles.description} ${styles.skeleton}`} />
          </div>
          <div className={styles.meta}>
            <span className={styles.domain}>{domain}</span>
          </div>
        </div>
        <div className={`${styles.thumbnail} ${styles.skeleton}`}>
          <div className={styles.placeholder} />
        </div>
      </div>
    </div>
  );
};

const LinkCardPresenter = (props: { metadata: OGPMetadata }) => {
  const domain = getDomain(props.metadata.url);
  const hasImage = !!props.metadata.image;

  return (
    <Link
      href={props.metadata.url}
      className={styles.container}
      target="_blank"
      rel="noopener noreferrer"
    >
      <div className={styles.card}>
        <div className={styles.content}>
          <div className={styles.text}>
            <h3 className={styles.title}>
              {props.metadata.title || props.metadata.url}
            </h3>
            {props.metadata.description && (
              <p className={styles.description}>{props.metadata.description}</p>
            )}
          </div>
          <div className={styles.meta}>
            {props.metadata.favicon && (
              <Image
                src={props.metadata.favicon}
                alt=""
                className={styles.icon}
                width={14}
                height={14}
                unoptimized
              />
            )}
            <span className={styles.domain}>{domain}</span>
          </div>
        </div>
        {hasImage && (
          <div className={styles.thumbnail}>
            <Image
              src={props.metadata.image!}
              alt=""
              fill
              className={styles.image}
              unoptimized
            />
          </div>
        )}
      </div>
    </Link>
  );
};

export const LinkCardClient = (props: Props) => {
  const { data: metadata, isLoading: loading } = useSWR(props.url, fetchOGP, {
    revalidateOnFocus: false,
    dedupingInterval: 60 * 1000,
  });

  if (loading || !metadata) {
    return <LinkCardSkeleton url={props.url} />;
  }

  return <LinkCardPresenter metadata={metadata} />;
};
