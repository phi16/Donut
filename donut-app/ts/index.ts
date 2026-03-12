import { App } from "./app";

async function main() {
  const wasm = await import("../pkg/index.js");
  new App(wasm as unknown as import("./wasm-api").WasmModule);
}

main().catch(console.error);
