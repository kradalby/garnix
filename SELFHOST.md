# Self-hosting garnix CI

This fork tracks [`OSSystems/garnix-ci:selfhost`](https://github.com/OSSystems/garnix-ci/tree/selfhost),
the community branch that strips garnix's defunct SaaS pieces (Stripe billing,
Hetzner, TOS), runs server hosting on self-managed microVMs instead, and ships a
deployable `nixosModules.garnix`. It is **GitHub-only**.

Upstream's `README.md`, `docs/*-selfhost.md` and the evaluated
`examples/example-selfhost.nix` are the reference. This doc covers the roles and
this fork's patches.

## The three roles

A garnix CI deployment splits into three roles. "Build" means two different
things in garnix — Nix realisation vs. garnix _actions_ — and they run on
different workers.

| Role               | What it is                                                                                                                                                                                                                                        | garnix software? | Count                                                    |
| ------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------- | -------------------------------------------------------- |
| **Coordinator**    | `services.garnixServer`: Haskell backend (GitHub webhooks → flake eval → orchestrate → status), Next.js frontend, nginx. Hard deps: **PostgreSQL** + **OpenSearch** (logs; fluent-bit ships to it, backend POSTs to the shipper's `:8888` input). | yes              | exactly 1                                                |
| **action-runner**  | Runs garnix **actions** (the run/test steps, e.g. NixOS-test / microVM work) inside **krun/KVM microVMs** (podman + crun/libkrun). Backend SSHes in as user `action-runner`.                                                                      | **yes**          | 1 (co-located by default; offloadable to one other host) |
| **remoteBuilders** | Plain **Nix remote builders**. `remoteBuilders.hosts` maps straight to `nix.buildMachines`.                                                                                                                                                       | **no**           | 0..N                                                     |

### remoteBuilders are NOT garnix machines

`remoteBuilders.hosts` entries map directly to NixOS `nix.buildMachines`. When the
list is non-empty the module sets `distributedBuilds = true` and forces the
coordinator's `max-jobs = 0`, so **all Nix realisation is offloaded** to these
hosts. SSH user defaults to `nix-ssh`. A builder needs only:

- a running `nix-daemon`,
- the coordinator's SSH public key authorized for a trusted build user,
- that user in `nix.settings.trusted-users`.

No garnix code runs on them — they're interchangeable with any nix builder you
already operate. Empty list → coordinator builds locally (`max-jobs = auto`).

The garnix-specific worker is the **action-runner**, which is separate from
realisation builders. It is co-located with the coordinator by default
(`actionRunner.host = "127.0.0.1"`) or runs standalone on one other host via
`nix/modules/action-runner.nix`, e.g. to move it off a low-RAM coordinator.

## Cache

garnix has its **own S3 binary cache**, driven by the backend (not by nix
substituter config):

- Gate: `services.garnixServer.s3Cache.enable` → `S3_CACHE_ENABLED` + `S3_CACHE_*`
  env. Options: `region` (default `auto`, so any S3-compatible store: AWS, MinIO,
  Garage, R2), `host`, `publicBucket`, `publicBaseUrl`, `privateBucket`.
- After eval/build the backend uploads new store paths as `<path>.nar.xz` +
  narinfo metadata, **signed** with the cache private key, into `publicBucket`
  (public repos) or `privateBucket` (private). Paths already in the NixOS/garnix
  caches are skipped; size-capped; Amazonka with retries.
- **Consumption is standard Nix and manual** — the module does _not_ auto-add
  substituters. Downstream machines add `publicBaseUrl` as a substituter + the
  cache public key as a trusted key. Builders run with
  `builders-use-substitutes = true` so they pull deps from substituters rather
  than copy from the coordinator.
- Off by default (no shared cache; outputs live in the coordinator's
  `/nix/store`). Enable it once you want a shared cache.

## Topology

Start single-node: one coordinator with co-located action-runner, local postgres
and opensearch, building locally. Scale out by:

1. adding existing nix builders to `remoteBuilders.hosts`,
2. offloading the action-runner to a second host (only if the coordinator is
   RAM-starved),
3. enabling the S3 cache (e.g. MinIO).

The module also supports splitting postgres / opensearch onto their own nodes
(`nix/modules/database.nix`, `opensearch/nixos-module.nix`) — see
`docs/opensearch-selfhost.md` and `docs/monitoring-selfhost.md`.

## Secrets

`services.garnixServer.secrets.*Path` take plain file paths, so any secret
manager works (sops-nix, agenix, Vault, …). Required:

- `databasePasswordPath`
- GitHub App: `githubAppIdPath`, `githubAppPkPath` (RSA PEM),
  `githubClientIdPath`, `githubClientSecretPath`, `githubWebhookSecretPath`
- `opensearchCredentialPath`
- `jwtKeyPath` (base64 of 32 raw bytes)
- repo-secrets **age keypair**: `repoSecretsKeyPath`, `repoSecretsPubKeyPath`
- `actionRunnerSshPath` (backend → action-runner SSH key; its public half goes in
  `garnix.actionRunner.authorizedKey`)
- `remoteBuilderSshPath` — only when `remoteBuilders.hosts != []`
- S3 cache keys — only when `s3Cache.enable = true`

## Fork-local tuning (env only)

These come from this fork's patches and have no module option. `Environment` is
a systemd list, so a second definition concatenates — set them straight on the
unit:

```nix
systemd.services.garnixServer.serviceConfig.Environment = [
  # Only these owners may build. Unset/empty = allow all.
  "GARNIX_ALLOWED_OWNERS=kradalby,myorg"
  # Replaces the built-in default build set for repos with no garnix.yaml.
  "GARNIX_DEFAULT_CONFIG=/etc/garnix/default.yaml"
  # Concurrency caps. Defaults: build 50, eval 50, s3 upload 100, fod check 20.
  "GARNIX_NIX_BUILD_POOL_SIZE=8"
  "GARNIX_NIX_EVAL_POOL_SIZE=4"
  "GARNIX_S3_UPLOAD_POOL_SIZE=20"
  "GARNIX_FOD_CHECK_POOL_SIZE=8"
];
```

Set `GARNIX_NIX_BUILD_POOL_SIZE` near the builder's core count on a small box.
Unbounded dispatch oversubscribes memory, and because the build timeout wraps the
queue wait, backlogged builds time out for no reason.

The built-in default build set omits `darwinConfigurations` (no macOS
builders); a repo opts in through its own `garnix.yaml`.

## GitHub App setup

You need a GitHub App (production and testing). On the running coordinator's
`/garnix-admin` page, press **"Submit to GitHub"** to create one; it returns the
credentials to drop into the secrets above. Then enable the App on the repos you
want built. Webhooks drive builds — so the coordinator needs an HTTP endpoint
GitHub can reach.

## Smoke test

Upstream dropped the `nixos-compose` VM flow. Submit a test build against a
running instance as in `docs/development.md` ("Submitting a test build").

See `MAINTAINING.md` for how this fork tracks the upstream forks.
