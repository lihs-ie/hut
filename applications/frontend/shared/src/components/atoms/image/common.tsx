import Image from "next/image";

export type Props = {
  src: string;
  alt: string;
};

export const CommonImage = (props: Props) => (
  <Image
    src={props.src}
    alt={props.alt}
    width={0}
    height={0}
    sizes="100vw"
    style={{ width: "100%", height: "100%", objectFit: "cover", margin: 0 }}
  />
);
