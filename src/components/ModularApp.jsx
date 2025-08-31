import React, { useState } from 'react';
import BitcrusherModule from './BitcrusherModule';
import TunerModule from './TunerModule';

export default function ModularApp() {
  const [health, setHealth] = useState('unknown');
  const [activeModules, setActiveModules] = useState(['bitcrusher']);

  const ping = async () => {
    const r = await fetch('/api/health');
    const j = await r.json();
    setHealth(j.status);
  };

  const toggleModule = (module) => {
    setActiveModules(prev => 
      prev.includes(module) 
        ? prev.filter(m => m !== module)
        : [...prev, module]
    );
  };

  return (
    <div style={{ fontFamily: 'system-ui', padding: 16 }}>
      <header style={{ marginBottom: 24, textAlign: 'center' }}>
        <h1>🎚️ Taku Modular Audio Processing</h1>
        <p>Modular Audio Framework: React → FastAPI → Rust</p>
        
        <div style={{ display: 'flex', justifyContent: 'center', gap: 8, marginTop: 12 }}>
          <button onClick={ping} style={{ padding: '4px 8px' }}>Backend Health</button>
          <span>Status: {health}</span>
        </div>
      </header>

      <nav style={{ 
        display: 'flex', 
        justifyContent: 'center', 
        gap: 12, 
        marginBottom: 24,
        padding: 16,
        backgroundColor: '#f8f9fa',
        borderRadius: 8
      }}>
        <label style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
          <input 
            type="checkbox" 
            checked={activeModules.includes('bitcrusher')}
            onChange={() => toggleModule('bitcrusher')}
          />
          Bitcrusher
        </label>
        <label style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
          <input 
            type="checkbox" 
            checked={activeModules.includes('tuner')}
            onChange={() => toggleModule('tuner')}
          />
          Universal Tuner
        </label>
      </nav>

      <main style={{ 
        display: 'grid', 
        gridTemplateColumns: 'repeat(auto-fit, minmax(400px, 1fr))', 
        gap: 16 
      }}>
        {activeModules.includes('bitcrusher') && <BitcrusherModule />}
        {activeModules.includes('tuner') && <TunerModule />}
      </main>

      {activeModules.length === 0 && (
        <div style={{ 
          textAlign: 'center', 
          padding: 32, 
          opacity: 0.6 
        }}>
          Select modules above to start processing audio
        </div>
      )}
    </div>
  );
}
