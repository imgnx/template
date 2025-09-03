import React, { useState } from 'react';
import BitcrusherModule from './components/BitcrusherModule';

export default function BitcrusherApp() {
  const [health, setHealth] = useState('unknown');

  const ping = async () => {
    const r = await fetch('/api/health');
    const j = await r.json();
    setHealth(j.status);
  };

  return (
    <div style={{ fontFamily: 'system-ui', padding: 16 }}>
      <header style={{ marginBottom: 24, textAlign: 'center' }}>
        <h1>🎛️ Taku Bitcrusher</h1>
        <p>Lo-fi Audio Processing: React → FastAPI → Rust</p>
        
        <div style={{ display: 'flex', justifyContent: 'center', gap: 8, marginTop: 12 }}>
          <button onClick={ping} style={{ padding: '4px 8px' }}>Backend Health</button>
          <span>Status: {health}</span>
        </div>
      </header>

      <main style={{ display: 'flex', justifyContent: 'center' }}>
        <BitcrusherModule />
      </main>
    </div>
  );
}
