import React from "react";
import { Berlin } from "@/utils/fonts";
import { Text } from "@/components/text";
import styles from "./styles.module.css";

export type InputProps<T> = {
  value: T;
  onChange: (t: T) => void;
};

export const TextInput = React.forwardRef(
  (
    props: InputProps<string> & { label?: string } & Omit<
        React.InputHTMLAttributes<HTMLInputElement>,
        "onChange" | "value"
      >,
    ref: React.ForwardedRef<HTMLInputElement>,
  ) => (
    <label className={`${styles.root} ${props.className}`}>
      {props.label && <Text>{props.label}</Text>}
      <input
        {...props}
        style={{ opacity: props.disabled ? 0.5 : 1 }}
        className={`${Berlin.className} ${styles.input}`}
        onChange={(e) => props.onChange(e.target.value)}
        ref={ref}
      />
    </label>
  ),
);
TextInput.displayName = "TextInput";

type SelectProps = {
  label: string;
  value: string;
  onChange: (t: string) => void;
  children: React.ReactNode;
};

export const Select = (props: SelectProps) => (
  <label className={styles.root}>
    <Text>{props.label}</Text>
    <select
      className={`${Berlin.className} ${styles.input}`}
      value={props.value}
      onChange={(e) => props.onChange(e.target.value)}
    >
      {props.children}
    </select>
  </label>
);

export const IntInput = (props: InputProps<number> & { label?: string }) => {
  return (
    <label className={styles.root}>
      <Text>{props.label}</Text>
      <UnstyledIntInput
        className={`${Berlin.className} ${styles.input}`}
        value={props.value}
        onChange={props.onChange}
      />
    </label>
  );
};

export const UnstyledIntInput = (
  props: InputProps<number> &
    Omit<React.InputHTMLAttributes<HTMLInputElement>, "onChange" | "value">,
) => {
  const [value, setValue] = React.useState<string>(`${props.value}`);
  const [prevPropValue, setPrevPropValue] = React.useState(props.value);
  if (props.value !== prevPropValue) {
    setPrevPropValue(props.value);
    setValue(`${props.value}`);
  }
  return (
    <input
      type="number"
      {...props}
      value={value}
      onChange={(e) => {
        setValue(e.target.value);
        const n = parseInt(e.target.value);
        if (!isNaN(n)) props.onChange(n);
      }}
      onBlur={() => setValue(`${props.value}`)}
    />
  );
};
