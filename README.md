# Taku Modular Audio Processing

A modular audio processing framework supporting multiple audio effects and tools. Currently includes:
- **Bitcrusher**: Lo-fi audio degradation effects
- **Universal Tuner**: Real-time pitch detection and tuning (in development)
- **Modular UI**: Combined interface for multiple modules

**Stack**: React (Webpack) • FastAPI • Rust (CLI/DSP)

## 🚀 Taku Build System (LAUNCHER.lisp)

The Taku framework includes a comprehensive LISP-based build system for managing all components:

```bash
# Quick start - development environment
./LAUNCHER.lisp dev                    # Start all modules (modular UI)
./LAUNCHER.lisp dev bitcrusher         # Start bitcrusher only
./LAUNCHER.lisp dev tuner              # Start tuner only

# Build system
./LAUNCHER.lisp build rust             # Build Rust components
./LAUNCHER.lisp build frontend         # Build all frontend targets
./LAUNCHER.lisp build all              # Build everything

# Code generation
./LAUNCHER.lisp generate reverb rust-crate           # Generate new Rust DSP module
./LAUNCHER.lisp generate reverb frontend-component   # Generate new React component
./LAUNCHER.lisp generate reverb api-endpoints        # Generate FastAPI endpoints

# System management
./LAUNCHER.lisp status                 # Check system health
./LAUNCHER.lisp clean                  # Clean build artifacts
./LAUNCHER.lisp tmux bitcrusher        # Launch in tmux session
```

## Module Architecture

- **🎛️ Bitcrusher**: Lo-fi degradation effects (active)
- **🎵 Tuner**: Real-time pitch detection (active) 
- **🌊 Reverb**: Space and ambience effects (planned)
- **🎚️ EQ**: Parametric equalizer (planned)

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
