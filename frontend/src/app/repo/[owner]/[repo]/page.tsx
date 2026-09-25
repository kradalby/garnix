"use client";

import React from "react";
import { WithSidebar } from "@/components/withSidebar";
import { CommitList } from "@/components/commitList";

const Page = (props: { params: Promise<{ owner: string; repo: string }> }) => {
  const params = React.use(props.params);
  return (
    <WithSidebar>
      <CommitList for={params} />
    </WithSidebar>
  );
};

export default Page;
