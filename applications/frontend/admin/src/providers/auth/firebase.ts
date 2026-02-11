import { getApp, getApps, initializeApp, type FirebaseApp } from "firebase/app";
import { connectAuthEmulator, getAuth, type Auth } from "firebase/auth";

const useEmulator = process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR === "true";

const firebaseConfig = {
  apiKey: useEmulator
    ? "demo-api-key"
    : process.env.NEXT_PUBLIC_FIREBASE_API_KEY,
  authDomain: useEmulator
    ? "demo-hut.firebaseapp.com"
    : process.env.NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN,
  projectId: useEmulator
    ? "demo-hut"
    : process.env.NEXT_PUBLIC_FIREBASE_PROJECT_ID,
  storageBucket: useEmulator
    ? "demo-hut.appspot.com"
    : process.env.NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET,
  messagingSenderId: useEmulator
    ? "000000000000"
    : process.env.NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID,
  appId: useEmulator ? "demo-app-id" : process.env.NEXT_PUBLIC_FIREBASE_APP_ID,
};

const emulatorConfig = {
  authHost: "localhost",
  authPort: 9099,
};

let authInstance: Auth | null = null;
let authEmulatorConnected = false;

const getFirebaseApp = (): FirebaseApp => {
  if (getApps().length > 0) {
    return getApp();
  }

  return initializeApp(firebaseConfig);
};

const getAuthInstance = (): Auth => {
  if (authInstance === null) {
    const app = getFirebaseApp();

    authInstance = getAuth(app);

    if (useEmulator && !authEmulatorConnected) {
      connectAuthEmulator(
        authInstance,
        `http://${emulatorConfig.authHost}:${emulatorConfig.authPort}`,
        { disableWarnings: true },
      );
      authEmulatorConnected = true;
    }
  }

  return authInstance;
};

export const FirebaseAuthProvider = {
  auth: {
    instance: getAuthInstance(),
  },
} as const;
