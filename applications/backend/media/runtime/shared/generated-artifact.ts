/** Rejects the checked-in placeholder so that it can never serve as production code. */
export function rejectStubArtifact(workerName: string, generatedArtifactKind: string): void {
  if (generatedArtifactKind === "stub") {
    throw new Error(
      `${workerName} is using the checked-in WASM/JSFFI stub. ` +
        "Build the Haskell executable before deploying or serving this Worker.",
    );
  }
}
