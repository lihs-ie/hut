import type { Metadata } from "next";
import { Noto_Sans_JP, Inter, Geist_Mono } from "next/font/google";
import { NuqsAdapter } from "nuqs/adapters/next/app";
import "@shared/global.css";
import { Header } from "@shared/components/organisms/header";
import { Footer } from "@shared/components/organisms/footer";
import { currentTheme } from "@shared/actions/theme";
import { getProfile } from "@shared/actions/admin";
import { NavigationProvider } from "@shared/components/molecules/navigation/provider";
import { ToastProvider } from "@shared/components/molecules/toast";

const notoSansJP = Noto_Sans_JP({
  variable: "--font-noto-sans-jp",
  subsets: ["latin"],
  weight: ["300", "400", "500", "700", "800", "900"],
});

const inter = Inter({
  variable: "--font-inter",
  subsets: ["latin"],
});

const geistMono = Geist_Mono({
  variable: "--font-geist-mono",
  subsets: ["latin"],
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
    index: true,
    follow: true,
  },
};

export default async function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  const theme = await currentTheme();

  return (
    <html lang="ja" className={theme}>
      <body className={`${notoSansJP.variable} ${inter.variable} ${geistMono.variable}`}>
        <NuqsAdapter>
          <ToastProvider>
            <NavigationProvider>
              <Header />
              <main>{children}</main>
              <Footer getProfile={getProfile} />
            </NavigationProvider>
          </ToastProvider>
        </NuqsAdapter>
      </body>
    </html>
  );
}
