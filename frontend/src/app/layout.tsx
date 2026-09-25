import { Metadata } from "next";
import { headers } from "next/headers";
import { Body } from "@/components/body";
import { Providers } from "@/store/providers";
import opengraph from "./og.png";
import "@/utils/colors.css";
import "@/utils/sizes.css";
import "./globals.css";

const baseMetadata = {
  title: "garnix | the nix CI",
  description: "Simple, fast, and green CI and caching for nix projects",
  images: [opengraph.src],
  metadataBase: new URL("https://garnix.io"),
};

export const metadata: Metadata = {
  ...baseMetadata,
  alternates: {
    types: {
      "application/rss+xml": [{ title: "Garnix Blog", url: "/feed.xml" }],
    },
  },
  twitter: baseMetadata,
  openGraph: baseMetadata,
};

const scriptifyFunction = (fn: () => void) => ({
  __html: `(${fn.toString()})()`,
});

const plausibleInit = scriptifyFunction(() => {
  window.plausible =
    window.plausible ||
    function () {
      // @ts-ignore
      (window.plausible.q = window.plausible.q || []).push(arguments);
    };
});

const RootLayout = async ({ children }: { children: React.ReactNode }) => {
  const nonce = (await headers()).get("x-nonce") || undefined;
  return (
    <html lang="en">
      <head>
        <script
          defer
          data-domain="garnix.io"
          src="https://plausible.io/js/script.outbound-links.tagged-events.js"
          nonce={nonce}
          suppressHydrationWarning
        />
        <script
          id="plausible"
          nonce={nonce}
          suppressHydrationWarning
          dangerouslySetInnerHTML={plausibleInit}
        />
      </head>
      <Providers>
        <Body>{children}</Body>
      </Providers>
    </html>
  );
};

export default RootLayout;
