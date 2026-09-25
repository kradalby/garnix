import fs from "node:fs/promises";
import { execFileSync } from "child_process";
import assert from "node:assert";
import test from "node:test";

const exec = (binName, ...argv) => execFileSync(binName, argv, {}).toString();

const wasmPackage = process.argv[2];

// In the browser this should just be fetch(<wasm url>), but since this
// is a local file we simulate this for this test:
const fakeWasmFetch = () =>
  fs.readFile(`${wasmPackage}/age.wasm`).then(
    (bytes) =>
      new Response(bytes, {
        headers: { "Content-Type": "application/wasm" },
      }),
  );

test("happy path", async () => {
  const { initAgeWasm } = await import(wasmPackage + "/index.js");
  const { encrypt } = await initAgeWasm(fakeWasmFetch());
  const [_, pubKey, privKey] = exec("age-keygen")
    .replace(/# public key: /, "")
    .split("\n");
  const result = encrypt(pubKey, "hunter2");
  assert.equal(result.ok, true);
  await fs.writeFile("encrypted-data", result.data);
  await fs.writeFile("age-priv-key", privKey);
  const decrypted = exec(
    "age",
    "--decrypt",
    "-i",
    "age-priv-key",
    "encrypted-data",
  );
  assert.equal(decrypted, "hunter2");
});

test("bad number of arguments", async () => {
  const { initAgeWasm } = await import(wasmPackage + "/index.js");
  const { encrypt } = await initAgeWasm(fakeWasmFetch());
  const result = encrypt();
  assert.deepEqual(result, {
    ok: false,
    error: "expected 2 arguments",
  });
});

test("bad age key passed in", async () => {
  const { initAgeWasm } = await import(wasmPackage + "/index.js");
  const { encrypt } = await initAgeWasm(fakeWasmFetch());
  const result = encrypt("not a valid age key", "hunter2");
  assert.deepEqual(result, {
    ok: false,
    error: "error at line 1: unknown recipient type",
  });
});
