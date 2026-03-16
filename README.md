# Donut

**Donut** is a programming language for composing higher-dimensional cells.

Higher-dimensional category theory provides a framework where programs are built by declaring and composing **cells** of various dimensions:

| Dimension | Role | Example |
|-----------|------|---------|
| 0-cell | Object / context | `u: *` |
| 1-cell | Morphism / type | `x: u -> u` |
| 2-cell | Program / operation | `m: x x -> x` |
| 3-cell | Equation / rewrite | `a: m x; m -> x m; m` |

Cells are composed using dimension-aware operators: space (parallel), `;` (sequential), `;;` (higher). The type checker verifies that boundaries match at every dimension.

```
u: *
x: u -> u
m: x x -> x
e: u -> x
a: m x; m -> x m; m
l: e x; m -> x
r: x e; m -> x
```

Donut also supports concrete computation via `import "sys"`, allowing numerical programs (f32, bool, vectors) to coexist with abstract categorical structures through **functors**.

## Structure

```
crates/
  donut-core/       -- Core cell calculus (PureCell, FreeCell, Prim, etc.)
  donut-lang/       -- Language frontend (tokenize -> parse -> convert -> resolve -> check -> deco)
  donut-layout/     -- Constraint-based layout solver for cell diagrams
  donut-renderer/   -- Canvas 2D renderer
  donut-runtime/    -- Evaluator and GLSL compiler
  donut-analysis/   -- Language server support (hover, completion, marking)
donut-app/          -- WASM web application (webpack + wasm-pack)
donut-ls/           -- Language Server Protocol implementation
docs/               -- Language guide (mdBook)
```

## Build

Requirements: Rust, wasm-pack, Node.js

```sh
# Web app
cd donut-app
npm install
npm start        # dev server
npm run build    # production build -> dist/

# Docs
cd docs
mdbook serve     # dev server
mdbook build     # build -> book/

# Tests
cargo test --workspace
```

## Deployment

Pushing to `main` triggers GitHub Actions to build and deploy to GitHub Pages:
- `/` -- Donut app
- `/docs/` -- Language guide

## Credits

This project is developed by [phi16](https://github.com/phi16).

Large portions of the codebase, documentation, and tooling were written with significant assistance from [Claude Code](https://claude.ai/claude-code) (Anthropic). The language design, type system implementation, layout solver, renderer, runtime, language server, and this deployment pipeline were all built through extensive human-AI collaboration.

## License

MIT
