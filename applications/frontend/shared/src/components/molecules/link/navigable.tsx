"use client";

import Link from "next/link";
import { useNavigation } from "@shared/components/molecules/navigation/provider";

type Props = {
  href: string;
  children: React.ReactNode;
  className?: string;
  target?: string;
  "aria-label"?: string;
};

export const NavigableLink = (props: Props) => {
  const { startNavigation } = useNavigation();

  const handleClick = () => {
    if (props.href.startsWith("#")) {
      return;
    }
    if (props.target === "_blank") {
      return;
    }
    startNavigation();
  };

  return (
    <Link
      href={props.href}
      className={props.className}
      target={props.target}
      onClick={handleClick}
      aria-label={props["aria-label"]}
    >
      {props.children}
    </Link>
  );
};
