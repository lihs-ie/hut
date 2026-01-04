import { Tag } from "@/domains/common";
import styles from "./tag.module.css";
import Image from "next/image";

export type Props = {
  tag: Tag;
};

const logoMap: Map<Tag, string> = new Map([
  [Tag.NEXT_JS as Tag, "/logos/nextjs.png"],
  [Tag.TYPESCRIPT as Tag, "/logos/typescript.png"],
  [Tag.REACT as Tag, "/logos/react.png"],
  [Tag.RUST as Tag, "/logos/rust.png"],
  [Tag.OTHER as Tag, "/logos/other.png"],
]);

export const TagLogo = (props: Props) => (
  <div className={styles.container}>
    <Image
      src={logoMap.get(props.tag)!}
      alt={props.tag}
      width={50}
      height={50}
    />
  </div>
);
