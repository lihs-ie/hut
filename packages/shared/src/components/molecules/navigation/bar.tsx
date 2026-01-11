"use client";

import { NavigationLinks } from "./links";
import styles from "./bar.module.css";
import { HamburgerIcon } from "@shared/components/atoms/icon/hamburger";
import { useState } from "react";
import { CrossIcon } from "@shared/components/atoms/icon/cross";

export const NavigationBar = () => {
  const [open, setOpen] = useState(false);

  const toggleMenu = () => {
    setOpen(!open);
  };

  return (
    <div className={styles.container}>
      <div className={`${styles.links} ${open && styles.active}`}>
        <NavigationLinks />
      </div>
      <div className={styles.icon} onClick={toggleMenu}>
        {open ? <CrossIcon /> : <HamburgerIcon />}
      </div>
    </div>
  );
};
