import React from "react";
import { createPortal } from "react-dom";

const noopSubscribe = () => () => {};

type Props = {
  enable?: boolean;
};

export const Portal = ({
  enable = true,
  children,
}: React.PropsWithChildren<Props>) => {
  const mounted = React.useSyncExternalStore(
    noopSubscribe,
    () => true,
    () => false,
  );
  return enable && mounted && createPortal(<>{children}</>, document.body);
};
