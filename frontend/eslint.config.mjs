import stylistic from "@stylistic/eslint-plugin";
import typescriptEslint from "@typescript-eslint/eslint-plugin";
import nextCoreWebVitals from "eslint-config-next/core-web-vitals";
import cssModules from "eslint-plugin-css-modules";

const config = [
  {
    ignores: [
      ".next/",
      "age-wasm/",
      "src/age-wasm-compiled",
      "src/components/blogPage/posts/JokeWebsite/",
    ],
  },
  ...nextCoreWebVitals,
  {
    plugins: {
      "@stylistic": stylistic,
      "@typescript-eslint": typescriptEslint,
      "css-modules": cssModules,
    },
    languageOptions: {
      parserOptions: {
        project: ["./tsconfig.json"],
      },
    },
    rules: {
      "import/order": [
        "error",
        {
          groups: [
            "builtin",
            "external",
            "internal",
            "parent",
            "sibling",
            "index",
            "object",
            "type",
          ],
        },
      ],
      "no-var": "error",
      "prefer-const": "error",
      "@stylistic/comma-dangle": [
        "error",
        {
          arrays: "always-multiline",
          objects: "always-multiline",
          imports: "always-multiline",
          exports: "always-multiline",
          functions: "always-multiline",
          // `<T,>` is how TSX spells a generic arrow function.
          generics: "ignore",
        },
      ],
      "react-hooks/exhaustive-deps": "error",
      "react/no-unescaped-entities": "off",
      "css-modules/no-unused-class": "error",
      "css-modules/no-undef-class": "error",
      "@typescript-eslint/no-unused-vars": [
        "error",
        {
          argsIgnorePattern: "^_",
          destructuredArrayIgnorePattern: "^_",
        },
      ],
      "@typescript-eslint/no-floating-promises": "error",
      "@typescript-eslint/no-misused-promises": "error",
    },
  },
];

export default config;
