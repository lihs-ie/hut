import Image from "next/image";
import styles from "./content.module.css";

type Props = {
  src: string;
  alt: string;
};

export const ContentImage = (props: Props) => (
  <Image
    className={styles.container}
    src={props.src}
    alt={props.alt}
    width={800}
    height={450}
    sizes="(max-width: 768px) 100vw, 800px"
    style={{ maxWidth: "100%", height: "auto" }}
  />
);
