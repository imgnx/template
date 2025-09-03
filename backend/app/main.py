from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel

app = FastAPI(title="Taku Bitcrusher API", version="0.1.0")

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


class CrushParams(BaseModel):
    bits: int = 12
    rate_hz: int = 16000


@app.get("/api/health")
async def health():
    return {"status": "ok"}


@app.post("/api/bitcrush")
async def bitcrush(params: CrushParams):
    # TODO: Accept audio data (e.g., WAV bytes or float32 PCM) and pass to Rust.
    # For now, echo back.
    return {"bits": params.bits, "rate_hz": params.rate_hz, "note": "stub"}
