import { useSyncExternalStore } from "react";

function featureLocalStorageName(featureName: string) {
  return `garnixfeature${featureName}`;
}

export function featureFlag(name: string): boolean {
  return (
    typeof window == "object" &&
    window.localStorage.getItem(featureLocalStorageName(name)) === "true"
  );
}

if (typeof window == "object") {
  // @ts-ignore
  window.__garnixSetFeatureFlag = (name: string, value: boolean) => {
    window.localStorage.setItem(
      featureLocalStorageName(name),
      JSON.stringify(value),
    );
  };
}

const noopSubscribe = () => () => {};

export function useFeatureFlag(name: string): boolean {
  return useSyncExternalStore(
    noopSubscribe,
    () => featureFlag(name),
    () => false,
  );
}
