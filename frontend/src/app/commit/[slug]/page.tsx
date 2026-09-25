"use client";

import React from "react";
import { P, match } from "ts-pattern";
import { CommitBuildsSummary } from "@/components/build";
import { StatusIcon } from "@/components/statusIcon";
import { Text } from "@/components/text";
import { Link } from "@/components/link";
import { formatCommitSha, formatRunName, runUrl } from "@/utils/format";
import { useLoading } from "@/hooks/useLoading";
import { BuildStatus } from "@/services/build";
import { Err, Ok } from "@/services";
import { fromSecs } from "@/utils/duration";
import { getCommit } from "@/services/commit";
import styles from "./styles.module.css";

const Page = (props: { params: Promise<{ slug: string }> }) => {
  const params = React.use(props.params);
  const commit = useLoading(
    React.useCallback(() => getCommit(params.slug), [params.slug]),
    {
      poll: fromSecs(5),
      shouldPoll: (result) =>
        match(result)
          .with(Err(P._), () => true)
          .with(Ok({ summary: P.select() }), (summary) => summary.pending > 0)
          .exhaustive(),
    },
  );
  if (commit.loading) return null;
  return (
    <main className={styles.container}>
      {match(commit.data)
        .with(Ok(P.select()), (commit) => (
          <>
            <Text type="h1" className={styles.h1}>
              Commit [{formatCommitSha(commit.summary)}]
            </Text>
            <CommitBuildsSummary commit={commit.summary} />
            <div className={styles.modules}>
              {[...commit.builds, ...commit.runs].map((build) => (
                <Link key={build.id} href={runUrl(build)} variant="wrapper">
                  <div className={styles.module}>
                    <Text>{formatRunName(build)}</Text>
                    <div className={styles.status}>
                      <StatusIcon status={build.status} />
                      <Text className={styles.statusText}>
                        {getStatusText(build.status)}
                      </Text>
                    </div>
                  </div>
                </Link>
              ))}
            </div>
          </>
        ))
        .with(Err(P._), () => (
          <Text>
            Uh oh! No build matching that description could be found. Either it
            doesn&apos;t exist, or you don&apos;t have access to it.
          </Text>
        ))
        .exhaustive()}
    </main>
  );
};

const getStatusText = (status: BuildStatus): string => {
  if (status === "Failure") return "Failed";
  else return status;
};

export default Page;
