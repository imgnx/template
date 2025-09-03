import React, { useState } from 'react';

export default function App() {
  const [bits, setBits] = useState(12);
  const [rate, setRate] = useState(16000);
  const [health, setHealth] = useState('unknown');

  const ping = async () => {
    const r = await fetch('/api/health');
    const j = await r.json();
    setHealth(j.status);
  };

  return (
    <div style={{ fontFamily: 'system-ui', padding: 16 }}>
      <h1>Taku Audio Processing</h1>
      <p>Modular Audio Framework: React → FastAPI → Rust</p>
      <p><em>Current Module: Bitcrusher</em></p>

      <section>
        <label>Bits: {bits}</label>
        <input type="range" min="1" max="24" value={bits} onChange={e => setBits(+e.target.value)} />
      </section>

      <section>
        <label>Rate: {rate} Hz</label>
        <input type="range" min="100" max="44100" step="100" value={rate} onChange={e => setRate(+e.target.value)} />
      </section>

      <div style={{ display: 'flex', gap: 8, marginTop: 12 }}>
        <button onClick={ping}>Backend Health</button>
        <span>health: {health}</span>
      </div>

      <p style={{ opacity: 0.7, marginTop: 24 }}>
        TODO: wire /api/bitcrush to Rust DSP once ready.
      </p>
    </div>
  );
}
