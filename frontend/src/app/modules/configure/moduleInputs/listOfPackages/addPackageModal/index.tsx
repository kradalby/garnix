import React from "react";
import { Loading } from "@/components/loading";
import { useLoading } from "@/hooks/useLoading";
import { FloatingModal, ModalActions, ModalSection } from "@/components/modal";
import { InputProps, TextInput } from "@/components/input";
import { Text } from "@/components/text";
import { Ok } from "@/services";
import { Button } from "@/components/button";
import { Berlin } from "@/utils/fonts";
import styles from "./styles.module.css";

let nixpkgs: null | Promise<Array<string>> = null;
const loadNixpkgs = (): Promise<Array<string>> => {
  if (nixpkgs !== null) return nixpkgs;
  nixpkgs = import("./all-nix-pkgs").then((x) => x.allPkgs);
  return nixpkgs;
};

const MAX_RESULTS = 50;

const NixpkgsSearchInternal = (props: {
  filter: string;
  pkgSet: Set<string>;
  onAdd: (pkg: string) => void;
  onRemove: (pkg: string) => void;
}) => {
  const nixpkgs = useLoading(
    React.useCallback(async () => Ok(await loadNixpkgs()), []),
  );
  const filter = props.filter.trim();
  if (nixpkgs.loading)
    return (
      <div className={styles.loading}>
        <Loading />
        Downloading package list...
      </div>
    );
  if (!nixpkgs.data.ok) return <>Something went wrong loading nixpkgs</>;
  if (filter === "") return null;
  const matched = nixpkgs.data.data.filter((n) =>
    n.toLowerCase().startsWith(filter.toLowerCase()),
  );
  return (
    <>
      {matched.slice(0, MAX_RESULTS).map((pkg) => {
        const fullPkgName = `pkgs.${pkg}`;
        return (
          <div key={pkg} className={styles.pkg}>
            <span>{pkg}</span>
            {props.pkgSet.has(fullPkgName) ? (
              <Button
                onClick={() => props.onRemove(fullPkgName)}
                style="warning"
              >
                Remove
              </Button>
            ) : (
              <Button onClick={() => props.onAdd(fullPkgName)}>Add</Button>
            )}
          </div>
        );
      })}
      {matched.length > MAX_RESULTS && (
        <div className={`${Berlin.className} ${styles.tooManyMatches}`}>
          Only the first {MAX_RESULTS} results are shown, consider refining your
          search.
        </div>
      )}
    </>
  );
};

const NixpkgsSearch = (props: {
  filter: string;
  pkgSet: Set<string>;
  onAdd: (pkg: string) => void;
  onRemove: (pkg: string) => void;
}) => (
  <div className={styles.nixPkgsSearchResults}>
    <NixpkgsSearchInternal {...props} />
  </div>
);

export const AddPackageModal = (
  props: InputProps<Array<string>> & {
    onRequestClose: () => void;
  },
) => {
  const [filter, setFilter] = React.useState("");
  const pkgSet = React.useMemo(() => new Set(props.value), [props.value]);
  return (
    <FloatingModal onRequestClose={props.onRequestClose}>
      <ModalSection>
        <Text type="h1">Add packages</Text>
        <Text type="p">Search for a nix package</Text>
      </ModalSection>
      <ModalSection>
        <TextInput
          className={styles.nixPkgsSearchInput}
          placeholder="Search nixpkgs..."
          value={filter}
          onChange={setFilter}
          ref={(el) => {
            el?.focus();
          }}
        />
        <NixpkgsSearch
          filter={filter}
          pkgSet={pkgSet}
          onAdd={(pkg) => props.onChange([...props.value, pkg])}
          onRemove={(toRemove) =>
            props.onChange(props.value.filter((pkg) => pkg !== toRemove))
          }
        />
      </ModalSection>
      <ModalSection>
        <ModalActions align="right">
          <Button onClick={props.onRequestClose}>Done</Button>
        </ModalActions>
      </ModalSection>
    </FloatingModal>
  );
};
