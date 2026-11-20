# Plan: one artifact per line in docs install snippets (ZIO-199)

## Problem
Some install snippets in the docs group several artifacts in one
`libraryDependencies ++= Seq(...)` block. Readers who need only one module
must pick it out of the group. Single-line snippets also lack a comment.

## Goal
Every install snippet in `docs/` lists each published artifact on its own
`libraryDependencies += ...` line, with a one-line `//` comment above it.

## Steps
1. Split the four grouped blocks: `docs/index.md` (5 blocks), `docs/magnolia/index.md`,
   `docs/experimental/index.md`, `docs/scala-parallel-collections/index.md`.
2. Add a one-line comment above each single `+=` line that has none
   (`docs/index.md`, `docs/getting-started.md`, `docs/laws/index.md`,
   `docs/newtypes/assertions.md`, and the `docs/how-to/*.md` and `docs/tutorial/*.md` snippets).
3. Keep artifact names, `% Test` scopes, `@VERSION@` placeholders and all prose unchanged.
4. Verify: grep for `++= Seq` (must be empty), check each `+=` has a comment above,
   run `sbt fmt` and `sbt lint`, and run `sbt docs/mdoc` if memory allows.
5. Review the full diff against `origin/series/2.x`, then remove this file before the PR.

## Files
- docs/index.md
- docs/magnolia/index.md
- docs/experimental/index.md
- docs/scala-parallel-collections/index.md
- docs/getting-started.md
- docs/laws/index.md
- docs/newtypes/assertions.md
- docs/how-to/define-equal-ord-hash-debug-instances.md
- docs/how-to/test-instances-with-laws.md
- docs/how-to/use-zpure-for-state-and-errors.md
- docs/how-to/accumulate-validation-errors.md
- docs/how-to/create-smart-newtypes.md
- docs/how-to/combine-values-with-associative-and-identity.md
- docs/how-to/traverse-with-foreach.md
- docs/how-to/derive-debug-with-magnolia.md
- docs/tutorial/validate-an-order.md
