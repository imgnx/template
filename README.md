# Taku Modular Audio Processing

A modular audio processing framework supporting multiple audio effects and tools. Currently includes:
- **Bitcrusher**: Lo-fi audio degradation effects
- **Universal Tuner**: Real-time pitch detection and tuning (in development)
- **Modular UI**: Combined interface for multiple modules

**Stack**: React (Webpack) • FastAPI • Rust (CLI/DSP)

## Dev

### Individual Modules

1) **Bitcrusher Only**
<pre>
# Backend
python -m uvicorn app.main:app --reload --port 8000

# Frontend (Bitcrusher)
cd frontend && npm run dev:bitcrusher
</pre>

2) **Universal Tuner Only**
<pre>
# Backend  
python -m uvicorn app.main:app --reload --port 8000

# Frontend (Tuner)
cd frontend && npm run dev:tuner
</pre>

3) **Modular Interface (Both)**
<pre>
# Backend
python -m uvicorn app.main:app --reload --port 8000

# Frontend (Modular)
cd frontend && npm run dev:modular
</pre>

### CLI (optional)

<pre>
cd rust
cargo run -p taku_cli -- --bits 8 --rate 8000
</pre>

## LISP launcher

<pre>
# Development (with module selection)
sbcl --script taku-start.lisp dev --target modular    # Combined interface (default)
sbcl --script taku-start.lisp dev --target bitcrusher # Bitcrusher only
sbcl --script taku-start.lisp dev --target tuner      # Tuner only

# Individual components
sbcl --script taku-start.lisp backend                 # Backend only
sbcl --script taku-start.lisp frontend --target TARGET # Frontend only
sbcl --script taku-start.lisp build-frontend --target TARGET # Build frontend

# Build commands
sbcl --script taku-start.lisp build-rust              # Build Rust
sbcl --script taku-start.lisp run-cli --bits 8 --rate 16000 # Run CLI

# Tmux (side-by-side panes)
sbcl --script taku-start.lisp tmux --target modular   # Combined interface
sbcl --script taku-start.lisp tmux --target bitcrusher # Bitcrusher only
sbcl --script taku-start.lisp tmux --target tuner     # Tuner only
</pre>
