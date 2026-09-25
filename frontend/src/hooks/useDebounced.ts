import React from "react";

export const useDebounced = (fn: () => void, ms: number) => {
  const timeoutId = React.useRef<NodeJS.Timeout | undefined>(undefined);
  return {
    clear: () => {
      clearTimeout(timeoutId.current);
      timeoutId.current = undefined;
    },
    enqueue: () => {
      clearTimeout(timeoutId.current);
      timeoutId.current = setTimeout(fn, ms);
    },
  };
};
