# Maintaining this fork

garnix was open-sourced after the company shut down (git history erased), so the
canonical upstream is effectively dead — last push 2026-06-17. Real work is
decentralized across forks.

The live one is [`OSSystems/garnix-ci`](https://github.com/OSSystems/garnix-ci)
(`selfhost` branch; `znaniye/garnix-ci` now redirects there). It is built
directly on this fork's `selfhost` branch, so our commits are its ancestors.
"Keeping up to date" means fast-forwarding onto it.

## Branch layout

| Branch        | Tracks                          | Purpose                                                                                  |
| ------------- | ------------------------------- | ---------------------------------------------------------------------------------------- |
| `main`        | `garnix-io:main`                | Pristine upstream mirror (reference only).                                               |
| `selfhost`    | `ossystems:selfhost`            | Deployable GitHub-only base. Fast-forward only.                                          |
| `multi-forge` | `garnix-io:multi-forge-support` | Parked Gitea/Forgejo work. Unused unless we adopt a non-GitHub forge.                    |
| `integration` | local, based on `selfhost`      | What we deploy. `selfhost` + this fork's docs/patches, kept as a thin rebase-able stack. |

`SELFHOST.md` and `MAINTAINING.md` live only on `integration` so `selfhost` stays
a clean fast-forward of `ossystems/selfhost`.

## Remotes

```bash
git remote add upstream  https://github.com/garnix-io/garnix-ci   # main, multi-forge-support
git remote add ossystems https://github.com/OSSystems/garnix-ci   # selfhost
```

## Periodic update

```bash
git fetch --all --prune

# What's new?
git log --oneline selfhost..ossystems/selfhost
git log --oneline multi-forge..upstream/multi-forge-support

# Advance the tracking branches (fast-forward only — no merge commits)
git switch selfhost    && git merge --ff-only ossystems/selfhost
git switch multi-forge && git merge --ff-only upstream/multi-forge-support

# Replay our local docs/patches on top of the new selfhost
git switch integration && git rebase selfhost
```

Keep `integration` a thin, replayable stack on `selfhost`. Rebase (not merge) so
the patches stay visible and cheap to replay when selfhost moves.

Anything on `integration` that is generally useful belongs upstream in
`OSSystems/garnix-ci` as a PR. The stack shrinking is the point. Only the docs
and the darwin-less default build set are fork-only; the other patches are open
PRs (`gh pr list -R OSSystems/garnix-ci --author @me`). Once one merges, the
rebase drops it as already applied; if upstream reworked it, `git rebase --skip`
our copy. Patches upstream solved differently go the same way: the orphan
reconciler gave way to upstream's orphan sweep (`Garnix/Sweep.hs`).

Dependency upgrades (flake inputs, frontend packages, CI pins, the OpenSearch
2.x pin) sit above the patches as commits of their own. On the next update,
drop the ones the new `selfhost` already covers and redo the rest.

## The other fork

[`joegoldin/garnix-ci-selfhosted`](https://github.com/joegoldin/garnix-ci-selfhosted)
forked upstream independently (no shared commits with us). It carries features
neither of the other forks has: Gitea as a second forge, Authentik SSO gating,
server backups, artifacts, a web terminal, a per-repo Configure page.

OSSystems already borrowed its microVM provisioner. Cherry-pick from it per
feature; do not try to merge it wholesale.

## Combining multi-forge (deferred, GitHub-only means we skip it)

Only if we ever adopt Gitea/Forgejo — and prefer joegoldin's Gitea work over
`garnix-io:multi-forge-support`, which is an unfinished abstraction:

```bash
git switch integration
git cherry-pick ce94b043 cbe31db0   # the 2 multi-forge commits onto selfhost
```

Expect conflicts in shared backend files (`backend/src/Garnix/{Garnix,API,DB,
Types,Build,Monad,Orchestrator}.hs` and the sqitch plan): keep selfhost's
stripped structure and layer the `Forge`/Gitea additions on top.

## Pinning

Downstream (e.g. a nix-configs flake input) pins via `flake.lock`. Bump
deliberately after running the periodic update above.
