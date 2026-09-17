/** The Haskell build emits a compiled WASM reactor at each imported path. */
declare module "*.wasm" {
  const wasmModule: WebAssembly.Module;
  export default wasmModule;
}
