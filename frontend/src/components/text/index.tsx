import z from "zod";
import { Berlin, MatterSQMono } from "@/utils/fonts";
import { withPropCheck } from "@/utils/withPropCheck";
import styles from "./styles.module.css";

const PropSchema = z.object({
  id: z.string().optional(),
  type: z.enum(["p", "h1", "h2", "h3", "proper", "code", "span"]).optional(),
  className: z.string().optional(),
  children: z.custom<React.ReactNode>(),
  style: z.custom<React.CSSProperties>().optional(),
  "data-testid": z.string().optional(),
});

export const Text = withPropCheck(
  PropSchema,
  ({ type = "p", children, className, ...rest }) => {
    switch (type) {
      case "p":
        return (
          <p
            {...rest}
            className={`${styles.all} ${Berlin.className} ${styles.paragraph} ${className}`}
          >
            {children}
          </p>
        );

      case "h1":
        return (
          <h1
            {...rest}
            className={`${styles.all} ${styles.header} ${className}`}
          >
            {children}
          </h1>
        );
      case "h2":
        return (
          <h2
            {...rest}
            className={`${styles.all} ${styles.header2} ${className}`}
          >
            {children}
          </h2>
        );
      case "h3":
        return (
          <h3
            {...rest}
            className={`${styles.all} ${styles.header} ${styles.header3} ${className}`}
          >
            {children}
          </h3>
        );
      case "proper":
        return (
          <span
            {...rest}
            className={`${styles.all} ${styles.proper} ${className} ${MatterSQMono.className}`}
          >
            {children}
          </span>
        );
      case "code":
        return (
          <span
            {...rest}
            className={`${styles.all} ${styles.code} ${className} ${MatterSQMono.className}`}
          >
            {children}
          </span>
        );
      case "span":
        return (
          <span
            {...rest}
            className={`${styles.all} ${className} ${Berlin.className}`}
          >
            {children}
          </span>
        );
      default:
        return <div>Text type not found: {type}</div>;
    }
  },
);
