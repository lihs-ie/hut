import type { Metadata } from "next";
import { Geist, Geist_Mono } from "next/font/google";
import { NuqsAdapter } from "nuqs/adapters/next/app";
import "@shared/global.css";
import { Header } from "@shared/components/organisms/header";
import { Footer } from "@shared/components/organisms/footer";
import { currentTheme } from "@shared/actions/theme";
import { isAdmin, logout } from "@/actions/auth";
import { getProfile } from "@shared/actions/admin";

const geistSans = Geist({
  variable: "--font-geist-sans",
  subsets: ["latin"],
});

const geistMono = Geist_Mono({
  variable: "--font-geist-mono",
  subsets: ["latin"],
});

export const metadata: Metadata = {
  title: {
    default: "hut-landlord",
    template: "%s | hut-landlord",
  },
  description: "hut管理アプリケーション",
};

export default async function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  const theme = await currentTheme();

  return (
    <html lang="ja" className={theme}>
      <body className={`${geistSans.variable} ${geistMono.variable} admin`}>
        <NuqsAdapter>
          <Header isAdmin={isAdmin} logout={logout} />
          <main>{children}</main>
          <Footer getProfile={getProfile} />
        </NuqsAdapter>
      </body>
    </html>
  );
}
