{ pkgs, commonBuildInputs, rts }:

pkgs.stdenv.mkDerivation {
  name = "moc.wasm";
  src = ../src;
  buildInputs = commonBuildInputs pkgs ++ [
    pkgs.ocamlPackages.js_of_ocaml
    pkgs.ocamlPackages.js_of_ocaml-ppx
    pkgs.ocamlPackages.wasm_of_ocaml-compiler
    pkgs.binaryen
    pkgs.nodejs
  ];
  buildPhase = ''
    patchShebangs .
  '' + pkgs.lib.optionalString (rts != null) ''
    ./rts/gen.sh ${rts}/rts/
  '' + ''
    make moc.wasm
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp --verbose --dereference moc.wasm $out/bin/moc.wasm.js
    cp -r --dereference _build/default/js/moc_wasm.bc.wasm.assets $out/bin/
  '';
  dontStrip = true;
  doInstallCheck = true;
  installCheckPhase = ''
    cat > smoke.cjs <<'NODE'
    require.main.filename = process.env.OUT + "/bin/driver.cjs";
    require(process.env.OUT + "/bin/moc.wasm.js");

    const waitForCompiler = async () => {
      for (let attempt = 0; attempt < 200; attempt++) {
        const compiler = globalThis.Motoko;
        if (compiler && typeof compiler.compileWasm === "function") {
          return compiler;
        }
        await new Promise((resolve) => setTimeout(resolve, 25));
      }
      throw new Error("Motoko Wasm compiler did not initialize");
    };

    (async () => {
      const compiler = await waitForCompiler();
      if (typeof compiler.inspectMotoko !== "function") {
        throw new Error("Compact Motoko inspection API is missing");
      }

      const inspected = compiler.inspectMotoko(
        "inspect.mo",
        `import Prim "mo:prim"; module {
          public func inspect(target : Text) : actor {} {
            Prim.cyclesAdd(1);
            actor (target)
          };
        }`
      );
      if (!inspected.code || !inspected.code.hasActorUrl ||
          inspected.code.dotMembers[0] !== "cyclesAdd" ||
          inspected.code.immediateImports[0] !== "mo:prim") {
        console.error(JSON.stringify(inspected));
        throw new Error("Compact Motoko inspection returned incorrect facts");
      }

      compiler.saveFile(
        "main.mo",
        `shared({ caller = _installer }) persistent actor class Class<system>() = this {
          public query func ping() : async Nat32 { 4294967295 };
        }`
      );

      const result = compiler.compileWasm("ic", "main.mo");
      if (!result || !result.code || !result.code.wasm || !result.code.candid) {
        console.error(JSON.stringify(result));
        throw new Error("Motoko Wasm compiler smoke compile failed");
      }

      const wasm = result.code.wasm;
      const expectedHeader = [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00];
      if (!(wasm instanceof Uint8Array) ||
          wasm.byteOffset !== 0 || wasm.byteLength !== wasm.buffer.byteLength ||
          !expectedHeader.every((byte, index) => wasm[index] === byte) ||
          !WebAssembly.validate(wasm)) {
        throw new Error("Compiler returned invalid or aliased Wasm bytes");
      }

      const baseline = new Uint8Array(wasm);
      const second = compiler.compileWasm("ic", "main.mo").code.wasm;
      if (!baseline.every((byte, index) => wasm[index] === byte)) {
        throw new Error("A prior result changed after another compile");
      }
      if (!WebAssembly.validate(second)) {
        throw new Error("A repeated compile returned invalid Wasm");
      }
      const secondBaseline = new Uint8Array(second);
      wasm[0] ^= 0xff;
      wasm[Math.floor(wasm.length / 2)] ^= 0xff;
      wasm[wasm.length - 1] ^= 0xff;
      if (!secondBaseline.every((byte, index) => second[index] === byte)) {
        throw new Error("Returned compiler buffers alias one another");
      }
      globalThis.gc();
      if (!secondBaseline.every((byte, index) => second[index] === byte)) {
        throw new Error("A returned buffer changed after compiler GC");
      }

      structuredClone(wasm, { transfer: [wasm.buffer] });
      if (wasm.buffer.byteLength !== 0) {
        throw new Error("Transferred result was not detached");
      }
      const third = compiler.compileWasm("ic", "main.mo").code.wasm;
      if (!WebAssembly.validate(third)) {
        throw new Error("Transferring a result detached compiler-owned state");
      }

      console.log(
        "compiled smoke actor: wasm=" + third.length +
        " candid=" + result.code.candid.length
      );
    })().catch((error) => {
      console.error(error);
      process.exitCode = 1;
    });
    NODE
    OUT=$out node --expose-gc smoke.cjs
  '';
}
