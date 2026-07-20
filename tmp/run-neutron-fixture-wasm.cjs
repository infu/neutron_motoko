const fs = require("node:fs");
const crypto = require("node:crypto");
const path = require("node:path");

const motokoWrapper = require("/srv/shared/code/neutron/node_modules/motoko/lib/index.js").default;

const compilerDir = path.resolve(
  process.env.MOC_WASM_DIR || path.join(__dirname, "..", "result", "bin"),
);

require.main.filename = path.join(compilerDir, "driver.cjs");
require(path.join(compilerDir, "moc.wasm.js"));

const fixturePath =
  process.argv[2] || "/srv/shared/code/neutron/tmp/current-compile-input.json";

function toBuffer(value) {
  if (typeof value === "string") return Buffer.from(value, "utf8");
  return Buffer.from(value);
}

function sha256(value) {
  return crypto.createHash("sha256").update(toBuffer(value)).digest("hex");
}

setTimeout(() => {
  const compiler = globalThis.Motoko;
  if (!compiler) throw new Error("Motoko compiler did not initialize");

  const input = JSON.parse(fs.readFileSync(fixturePath, "utf8"));
  const mo = motokoWrapper(compiler);

  for (const file of input.modules || []) {
    mo.write(file.path, file.content);
  }
  mo.write("neutron.mo", input.neutron);

  const started = Date.now();
  const result = mo.wasm("neutron.mo", "ic");
  const elapsed = Date.now() - started;
  if (process.env.WRITE_OUTPUT_DIR) {
    fs.mkdirSync(process.env.WRITE_OUTPUT_DIR, { recursive: true });
    const stem = path.basename(fixturePath, ".json");
    fs.writeFileSync(path.join(process.env.WRITE_OUTPUT_DIR, `${stem}.wasm`), toBuffer(result.wasm));
    fs.writeFileSync(path.join(process.env.WRITE_OUTPUT_DIR, `${stem}.did`), result.candid);
    fs.writeFileSync(path.join(process.env.WRITE_OUTPUT_DIR, `${stem}.stable`), toBuffer(result.stable));
  }

  console.log(
    JSON.stringify(
      {
        ok: true,
        elapsedMs: elapsed,
        wasmBytes: result.wasm.length,
        wasmHash: sha256(result.wasm),
        candidBytes: result.candid.length,
        candidHash: sha256(result.candid),
        stableBytes: result.stable.length,
        stableHash: sha256(result.stable),
      },
      null,
      2,
    ),
  );
}, 1000);
