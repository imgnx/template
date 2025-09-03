#!/usr/bin/env bash
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)

printf "\n[1/3] Backend deps (FastAPI)\n"
cd "$here/backend"
python -m pip install -r requirements.txt

printf "\n[2/3] Frontend deps (React/Webpack)\n"
cd "$here/frontend"
if [ ! -f package-lock.json ]; then
  npm install
fi

printf "\n[3/3] Rust workspace build\n"
cd "$here/rust"
cargo build

printf "\nDone.\n"
