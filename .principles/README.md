# Principles

These rules govern docs, code organization, and every `AGENTS.md` in the tree. Nested files may add stricter rules; they cannot relax these.

## Pillars

Load-bearing concepts.

- **Invariant** — a property preserved across every change.
- **Locality** — information lives where it is used; effects have the smallest scope possible.
- **Modularity / separation of concerns** — each unit owns one concern; concerns don't cross boundaries.
- **Refinement** — a stronger spec entails a weaker one. Stricter, narrower, more specific — never the reverse.
- **Underspecification** — commit to only what's required; leave the rest open to refinement.
- **Incrementality** — change in small steps; each step preserves invariants.
- **DRY** — one canonical source per fact. Duplicates rot.
- **KISS** — pick the simpler form that meets the need.
- **Self-similar / fractal** — the same shape repeats at multiple scales.

## Self-contained

- Describe what is, not what was or will be.
- No historical text ("moved from", "previously", "is preserved", "now uses", "still").
- No inline anticipation of future changes.
- Track migrations and ports in flight explicitly — in committed docs or GitHub issues — never as asides in source files.
- For code: no `// removed for X` comments, no `_old` / `_legacy` prefixed names, no dead backwards-compat shims.

Example: "The sourcing pattern (not symlinks) is preserved." → "Configs are sourced, not symlinked."

## Recursively hierarchical

- Root holds repo-wide invariants and the top-level map.
- Nested `AGENTS.md`, modules, and packages cover local specifics.
- Don't restate child content in the parent.
- Don't restate parent content in the child.

The same applies to code organization and package layout.

## Document surprises only

- Structure and conventions repeat predictably across the tree.
- Document only the surprising.
- Say nothing if `ls`, file contents, naming conventions, or the directory's own `AGENTS.md` would answer the question.
- A map that names only surprises stays stable as contents change.

## Terse, precise, prescriptive

Write to be acted on, not interpreted.

- Use short, direct sentences.
- Reference concretely: file paths, function names, not "the helper" or "some script".
- Use the existing domain word. If a concept has an established name in software, math, or formal methods, use it instead of paraphrasing.
- Invented terms require an explicit definition and "quotes" on every appearance. Define inline at first; move to `GLOSSARY.md` (this directory) once the list grows.
- Use imperatives, not hedges. "Do X" beats "you might want to X".
- Name conditions. If a fact is conditional, say when.
- Reread for words that could mean two things, claims that omit where, instructions that omit when. Ambiguity is a defect.

## How to apply

- Fix violations you notice in files you are editing. Don't open separate cleanup PRs without user approval.
- Approved deviations live in `EXCEPTIONS.md` (this directory). New entries require explicit user approval in the current turn.
- These rules apply to this file. Fix violations when you see them.
