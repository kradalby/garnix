# Maintaining this fork

garnix was open-sourced after the company shut down (git history erased), so the
canonical upstream is effectively dead. Real work is decentralized across forks.
"Keeping up to date" means tracking the live forks — chiefly
[`znaniye/garnix-ci`](https://github.com/znaniye/garnix-ci) (the `selfhost`
branch) — not a moving `garnix-io/main`.

## Branch layout

| Branch | Tracks | Purpose |
|---|---|---|
| `main` | `garnix-io:main` | Pristine upstream mirror (reference only). |
| `selfhost` | `znaniye:selfhost` | Deployable GitHub-only base. Fast-forward only. |
| `multi-forge` | `garnix-io:multi-forge-support` | Parked Gitea/Forgejo work. Unused unless we adopt a non-GitHub forge. |
| `integration` | local, based on `selfhost` | What we deploy. `selfhost` + this fork's docs/patches, kept as a thin rebase-able stack. |

`SELFHOST.md` and `MAINTAINING.md` live only on `integration` so `selfhost` stays
a clean fast-forward of `znaniye/selfhost`.

## Remotes

```bash
git remote add upstream https://github.com/garnix-io/garnix-ci   # main, multi-forge-support
git remote add znaniye  https://github.com/znaniye/garnix-ci     # selfhost
```

## Periodic update

```bash
git fetch --all --prune

# What's new?
git log --oneline selfhost..znaniye/selfhost
git log --oneline multi-forge..upstream/multi-forge-support

# Advance the tracking branches (fast-forward only — no merge commits)
git switch selfhost    && git merge --ff-only znaniye/selfhost
git switch multi-forge && git merge --ff-only upstream/multi-forge-support

# Replay our local docs/patches on top of the new selfhost
git switch integration && git rebase selfhost
```

Keep `integration` a thin, replayable stack on `selfhost`. Rebase (not merge) so
the patches stay visible and cheap to replay when selfhost moves.

## Discovering new fork work

Work is spread across ~30 forks. Periodically sweep for branches with extra
commits (most forks are snapshots at the open-sourcing commit and carry nothing):

```bash
gh api repos/garnix-io/garnix-ci/forks --paginate \
  --jq '.[] | "\(.full_name) \(.pushed_at)"'        # who pushed recently?

# For a candidate fork, list its non-main branches and compare:
gh api repos/<owner>/garnix-ci/branches --jq '.[].name'
gh api repos/garnix-io/garnix-ci/compare/garnix-io:main...<owner>:<branch> \
  --jq '{ahead:.ahead_by, behind:.behind_by}'
```

As of the last sweep (2026-06), only `znaniye` (selfhost) and `Arvuno` (two small
fix branches, otherwise == main) carried extra work; `multi-forge-support` lives
on `garnix-io` itself.

## Combining multi-forge (deferred, GitHub-only means we skip it)

Only if we ever adopt Gitea/Forgejo:

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
