import type { Metadata } from "next";
import { Suspense } from "react";
import { Noto_Sans_JP, Geist_Mono } from "next/font/google";
import { NuqsAdapter } from "nuqs/adapters/next/app";
import "@shared/global.css";
import { Header } from "@shared/components/organisms/header";
import { Footer } from "@shared/components/organisms/footer";
import { FooterPresenter } from "@shared/components/organisms/footer/index.presenter";
import { FooterErrorBoundary } from "@shared/components/molecules/boundary/footer-error";
import { getProfile } from "@shared/actions/admin";
import { NavigationProvider } from "@shared/components/molecules/navigation/provider";
import { ToastProvider } from "@shared/components/molecules/toast";
import { ThemeProvider } from "@shared/components/molecules/theme/provider";

const notoSansJP = Noto_Sans_JP({
  variable: "--font-noto-sans-jp",
  subsets: ["latin"],
  weight: ["400", "500", "600", "700", "800", "900"],
  display: "swap",
});

const geistMono = Geist_Mono({
  variable: "--font-geist-mono",
  subsets: ["latin"],
  display: "swap",
  preload: false,
});

export const metadata: Metadata = {
  title: {
    default: "hut",
    template: "%s | hut",
  },
  description:
    "個人的な技術学習記事やメモを公開するためのプラットフォームです。",
  openGraph: {
    type: "website",

    locale: "ja_JP",
    siteName: "hut",
  },
  twitter: {
    card: "summary_large_image",
    creator: "@lihs_ie",
  },
  robots: {
    index: process.env.DISALLOW_ROBOTS !== "true",
    follow: process.env.DISALLOW_ROBOTS !== "true",
  },
  alternates: {
    types: {
      "application/rss+xml": new URL(
        "/feed.xml",
        process.env.NEXT_PUBLIC_SITE_URL ?? "https://hut.lihs.dev",
      ).toString(),
    },
  },
};

/**
 * Reader アプリ全体の共通レイアウトを返す。
 */
export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="ja" suppressHydrationWarning>
      <body className={`${notoSansJP.variable} ${geistMono.variable}`}>
        <ThemeProvider>
          <NuqsAdapter>
            <ToastProvider>
              <NavigationProvider>
                <Header />
                <main>{children}</main>
                <FooterErrorBoundary>
                  <Suspense fallback={<FooterPresenter mailAddress={null} externalServices={new Map()} />}>
                    <Footer getProfile={getProfile} />
                  </Suspense>
                </FooterErrorBoundary>
              </NavigationProvider>
            </ToastProvider>
          </NuqsAdapter>
        </ThemeProvider>
      </body>
    </html>
  );
}
