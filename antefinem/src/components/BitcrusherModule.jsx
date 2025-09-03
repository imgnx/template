import React, { useState } from 'react';

export default function BitcrusherModule() {
  const [bits, setBits] = useState(12);
  const [rate, setRate] = useState(16000);
  const [dspStatus, setDspStatus] = useState('');

  const startRustDSP = async () => {
    setDspStatus('Starting...');
    try {
      const r = await fetch('/api/bitcrush', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ bits, rate })
      });
      if (!r.ok) throw new Error('Failed to start DSP');
      const j = await r.json();
      setDspStatus(j.status || 'DSP started!');
    } catch (e) {
      setDspStatus('Error: ' + e.message);
    }
  };

  return (
    <div style={{ border: '2px solid #4A90E2', borderRadius: 8, padding: 16, margin: 8 }}>
      <h2>🎛️ Bitcrusher</h2>
      <p>Lo-fi audio degradation effects</p>

      <section style={{ marginBottom: 12 }}>
        <label>Bit Depth: {bits}</label>
        <input 
          type="range" 
          min="1" 
          max="24" 
          value={bits} 
          onChange={e => setBits(+e.target.value)}
          style={{ width: '100%', marginTop: 4 }}
        />
      </section>

      <section style={{ marginBottom: 12 }}>
        <label>Sample Rate: {rate} Hz</label>
        <input 
          type="range" 
          min="100" 
          max="44100" 
          step="100" 
          value={rate} 
          onChange={e => setRate(+e.target.value)}
          style={{ width: '100%', marginTop: 4 }}
        />
      </section>

      <div style={{ marginTop: 16 }}>
        <button 
          onClick={startRustDSP}
          style={{ 
            padding: '8px 16px', 
            backgroundColor: '#4A90E2', 
            color: 'white', 
            border: 'none', 
            borderRadius: 4,
            cursor: 'pointer'
          }}
        >
          Start Bitcrusher DSP
        </button>
        {dspStatus && (
          <span style={{ marginLeft: 12, opacity: 0.8 }}>{dspStatus}</span>
        )}
      </div>
    </div>
  );
}
