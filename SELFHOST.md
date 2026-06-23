# Self-hosting garnix CI

This fork tracks [`znaniye/garnix-ci:selfhost`](https://github.com/znaniye/garnix-ci/tree/selfhost),
the community branch that strips garnix's defunct SaaS pieces (Stripe billing,
Hetzner app-hosting, DNS, hosting-gateway, TOS) and ships a deployable
`nixosModules.garnix`. It is **GitHub-only**.

A real, working reference deployment lives at
[`znaniye/nix-configs`](https://github.com/znaniye/nix-configs)
(`modules/nixos/server/{garnix,garnix-runner}.nix`). Read it alongside this doc.

## The three roles

A garnix CI deployment splits into three roles. "Build" means two different
things in garnix — Nix realisation vs. garnix *actions* — and they run on
different workers.

| Role | What it is | garnix software? | Count |
|---|---|---|---|
| **Coordinator** | `services.garnixServer`: Haskell backend (GitHub webhooks → flake eval → orchestrate → status), Next.js frontend, nginx. Hard deps: **PostgreSQL** + **OpenSearch** (logs; fluent-bit ships to it, backend POSTs to the shipper's `:8888` input). | yes | exactly 1 |
| **action-runner** | Runs garnix **actions** (the run/test steps, e.g. NixOS-test / microVM work) inside **krun/KVM microVMs** (podman + crun/libkrun). Backend SSHes in as user `action-runner`. | **yes** | 1 (co-located by default; offloadable to one other host) |
| **remoteBuilders** | Plain **Nix remote builders**. `remoteBuilders.hosts` maps straight to `nix.buildMachines`. | **no** | 0..N |

### remoteBuilders are NOT garnix machines

`remoteBuilders.hosts` entries map directly to NixOS `nix.buildMachines`. When the
list is non-empty the module sets `distributedBuilds = true` and forces the
coordinator's `max-jobs = 0`, so **all Nix realisation is offloaded** to these
hosts. SSH user defaults to `nix-ssh` (znaniye uses `nixremote`). A builder needs
only:

- a running `nix-daemon`,
- the coordinator's SSH public key authorized for a trusted build user,
- that user in `nix.settings.trusted-users`.

No garnix code runs on them — they're interchangeable with any nix builder you
already operate. Empty list → coordinator builds locally (`max-jobs = auto`).

The garnix-specific worker is the **action-runner**, which is separate from
realisation builders. It can be co-located with the coordinator
(`localActionRunner = true`, loopback) or run standalone on one other host via
`nix/modules/action-runner.nix` (znaniye delegates from a low-RAM aarch64
coordinator to an x86_64 box).

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
- **Consumption is standard Nix and manual** — the module does *not* auto-add
  substituters. Downstream machines add `publicBaseUrl` as a substituter + the
  cache public key as a trusted key. Builders run with
  `builders-use-substitutes = true` so they pull deps from substituters rather
  than copy from the coordinator.
- The reference deployment runs with `s3Cache.enable = false` (no shared cache;
  outputs live in the coordinator's `/nix/store`). Enable it once you want a
  shared cache.

## Topology

Start single-node: one coordinator with co-located action-runner + local postgres
+ opensearch, building locally. Scale out by:

1. adding existing nix builders to `remoteBuilders.hosts`,
2. offloading the action-runner to a second host (only if the coordinator is
   RAM-starved),
3. enabling the S3 cache (e.g. MinIO).

The module also supports splitting postgres / opensearch onto their own nodes
(`nix/modules/{database,opensearch}`) — see `examples/example-multi-server-deployment.nix`.

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

## GitHub App setup

You need a GitHub App (production and testing). On the running coordinator's
`/garnix-admin` page, press **"Submit to GitHub"** to create one; it returns the
credentials to drop into the secrets above. Then enable the App on the repos you
want built. Webhooks drive builds — so the coordinator needs an HTTP endpoint
GitHub can reach.

## Local smoke test (VMs)

```bash
nix run -L .#examples_spinUpVms        # uses garnix-io/nixos-compose
nixos-compose tap
nixos-compose status
# browse the exampleGarnixServer IP; /garnix-admin has dev tooling
curl -v -XPOST \
  http://$(nixos-compose ip exampleGarnixServer)/api/build/submit \
  -H 'Content-Type: application/json' \
  -d '{ "owner": "garnix-io", "repo": "comment", "testCommit": "8b2b57d91dd1f4d094bb944a0a0ef65319a5663f" }'
# result under /repo/garnix-io/comment
```

See `MAINTAINING.md` for how this fork tracks the upstream forks.
