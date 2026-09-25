"use client";

import Image from "next/image";
import React from "react";
import { Text } from "@/components/text";
import dashIcon from "@/components/icons/dash.svg";
import { Link } from "@/components/link";
import crossIcon from "@/components/icons/cross.svg";
import { BuildWithRelatedBuilds } from "@/services/build";
import { StatusIcon } from "@/components/statusIcon";
import { formatCommitSha } from "@/utils/format";
import { LogStream, useLogStream } from "@/hooks/useLogStream";
import { styleLines } from "@/utils/ansi";
import { Loading } from "@/components/loading";
import styles from "./styles.module.css";

export const RunLog = (props: { runId: string }) => {
  const logStream = useLogStream("run", props.runId);
  return <LogViewer logStream={logStream} defaultLogGroupName="Run" />;
};

export const BuildLog = ({ build }: { build: BuildWithRelatedBuilds }) => {
  const logStream = useLogStream("build", build.id);
  if (build.status !== "Pending" && build.original_build != null) {
    return (
      <>
        Build skipped since this package has already been built in a previous
        commit.
        <div className={styles.otherBuilds}>
          <Link href={`/build/${build.original_build.id}`}>
            <StatusIcon status={build.original_build.status} />
            {formatCommitSha(build.original_build)}
          </Link>
        </div>
      </>
    );
  }
  return <LogViewer logStream={logStream} defaultLogGroupName="Unknown" />;
};

const LogViewer = (props: {
  logStream: LogStream;
  defaultLogGroupName: string;
}) => {
  const { logs } = props.logStream;
  const [openLog, setOpenLog] = React.useState<string | undefined>();
  const onlyLogGroupName = logs.length === 1 ? logs[0]?.[0] : undefined;
  const [prevOnlyLogGroupName, setPrevOnlyLogGroupName] = React.useState<
    string | undefined
  >();
  if (onlyLogGroupName !== prevOnlyLogGroupName) {
    setPrevOnlyLogGroupName(onlyLogGroupName);
    if (onlyLogGroupName != null) setOpenLog(onlyLogGroupName);
  }
  const toggleLog = (logGroupName: string) => {
    setOpenLog(openLog !== logGroupName ? logGroupName : undefined);
  };
  return (
    <>
      {logs.map(([logGroupName, logs]) => (
        <div
          key={logGroupName}
          className={`${styles.log} ${openLog === logGroupName && styles.open}`}
        >
          <div
            className={styles.logHead}
            onClick={() => toggleLog(logGroupName)}
          >
            <Text>{logGroupName || props.defaultLogGroupName}</Text>
            {openLog === logGroupName ? (
              <Image src={dashIcon} alt="close" className={styles.icon} />
            ) : (
              <Image src={crossIcon} alt="open" className={styles.icon} />
            )}
          </div>
          {openLog === logGroupName && <AnsiLogViewer logs={logs} />}
        </div>
      ))}
      {props.logStream.loading && (
        <div className={styles.loading}>
          <Loading />
        </div>
      )}
    </>
  );
};

const AnsiLogViewer = (props: { logs: Array<string> }) => {
  const logs = React.useMemo(() => styleLines(props.logs), [props.logs]);
  return (
    <div className={styles.logBody}>
      <pre className={styles.logBodyInner}>
        {logs.map((logLine, index) => (
          <div key={index}>
            {logLine.map(([style, text], idx) => (
              <span key={idx} style={style}>
                {text}
              </span>
            ))}
          </div>
        ))}
      </pre>
    </div>
  );
};
