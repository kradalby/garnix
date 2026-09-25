const withBundleAnalyzer = require("@next/bundle-analyzer")({
  enabled: process.env.ANALYZE === "true",
});

/** @type {import('next').NextConfig} */
const nextConfig = {
  output: "standalone",
  poweredByHeader: false,
  images: { unoptimized: true },
  reactStrictMode: false,
  skipTrailingSlashRedirect: true,
  // ESM-only; lets next/jest transform it for the CommonJS test runtime.
  transpilePackages: ["marked"],
  rewrites: () => {
    if (process.env.GARNIX_SERVER_ORIGIN != null) {
      return [
        {
          source: "/api/:path*",
          destination: `${process.env.GARNIX_SERVER_ORIGIN}/api/:path*`,
        },
      ];
    } else {
      return [];
    }
  },
  generateBuildId: async () => {
    return process.env.NEXT_BUILD_ID || "next-build";
  },
  webpack: (config, { buildId }) => {
    config.resolve.symlinks = false;
    config.output.filename = config.output.filename.replace(
      "[chunkhash]",
      `g-${buildId}`,
    );
    config.module.rules.push({
      test: /\.cast/,
      loader: "raw-loader",
    });
    config.module.rules.push({
      test: /\.wasm/,
      type: "asset/resource",
      generator: {
        filename: "static/chunks/[path][name].[hash][ext]",
      },
    });
    return config;
  },
};

module.exports = withBundleAnalyzer(nextConfig);
