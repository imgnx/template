# Rules of the Road

## Naming
- `antefinem`: frontend (React + Webpack), HTML templates under `in/`
- `retrofinem`: backend (FastAPI app under `app/`)
- `meta`: meta/tooling (formerly `adjacent`)

## Build Order (highest burndown first)
1) Frontend bundle (Webpack → `dist/`)
2) Backend server (FastAPI)
3) Native WebView (wry/winit)

Rationale: the native dev session depends on the bundled output; prod serving is file:// to `dist/index.html`.

## WebView Sanity Check
The Rust `dev_view` binary enforces these checks:

- Production (`TAKU_ENV=production` or `--prod`):
  - Must find `index.html` in `antefinem/dist/` or `dist/` → serve `file://…`
  - If missing, exit and prompt to run `npm run build` (or target-specific build)

- Development (default):
  - Look for a template in `antefinem/{in,src,public,http,www}/index.html` or root equivalents
  - If found → build (`npm run build:$BUILD_TARGET` or `npm run build`) → serve `dist/index.html`
  - If not found → scan top-level for `*.html`; if none, instruct to add `in/index.html`

## Environment
- `BUILD_TARGET` ∈ { modular (default), bitcrusher, tuner }
- `TAKU_ENV=production` forces production serving from `dist/`

## CI/CD (future)
- Lint/build frontend first; fail early if `dist/` missing
- Spin FastAPI for integration tests; run WebView smoke on packaged `dist/`
