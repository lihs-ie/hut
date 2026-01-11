import styles from "./page.module.css";
import { Login } from "./_components/organisms/login";

export default function AdminLoginPage() {
  return (
    <div className={styles.container}>
      <Login redirectURI="/" />
    </div>
  );
}
