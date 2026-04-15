import { fetchOGP, OGPMetadata } from "@shared/actions/ogp";
import styles from "./link.module.css";
import Image from "next/image";
import Link from "next/link";

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

export const LinkCard = async (props: Props) => {
  const metadata = await fetchOGP(props.url);

  return <LinkCardPresenter metadata={metadata} />;
};
