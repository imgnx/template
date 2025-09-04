# Usage

## Dev (tmux)
- `./LAUNCHER tmux modular` — backend + frontend + webview panes
- `./LAUNCHER tmux bitcrusher` — focus Bitcrusher target
- `./LAUNCHER tmux tuner` — focus Tuner target

## Dev (manual)
- Backend: `cd retrofinem && python -m uvicorn app.main:app --reload --port 8000`
- Frontend: `npm run dev:modular` (or `dev:bitcrusher`, `dev:tuner`)
- WebView: `cargo run -p taku_cli --bin dev_view`

## Production
- Build: `npm run build` (or `BUILD_TARGET=bitcrusher npm run build`)
- Launch: `TAKU_ENV=production cargo run -p taku_cli --bin dev_view`

## Sanity Check (built into dev_view)
- Prod: requires `dist/index.html` (or `antefinem/dist/index.html`) → serves file://
- Dev: requires a template in `antefinem/in/index.html` (or src/public/http/www) → builds → serves dist

## Environment
- `BUILD_TARGET` — `modular` (default), `bitcrusher`, `tuner`
- `TAKU_ENV` — `production` to force file:// serving

## Module builds (router-less encouragement)
- Build only one module's bundle:
  - `./LAUNCHER module bitcrusher`
  - `./LAUNCHER module tuner`
- This runs `npm run build:<module>` under the hood (no router required).
