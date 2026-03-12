import { App } from "./app";

async function main() {
  const wasm = await import("../pkg/index.js");
  new App(wasm as any);
}

main().catch(console.error);
