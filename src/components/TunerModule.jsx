import React, { useState } from 'react';

export default function TunerModule() {
  const [note, setNote] = useState('A4');
  const [frequency, setFrequency] = useState(440);
  const [detectedNote, setDetectedNote] = useState('');
  const [cents, setCents] = useState(0);
  const [tunerStatus, setTunerStatus] = useState('');

  const notes = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
  const octaves = [2, 3, 4, 5, 6, 7];

  const startTuner = async () => {
    setTunerStatus('Starting tuner...');
    try {
      const r = await fetch('/api/tuner/start', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ reference_note: note, reference_freq: frequency })
      });
      if (!r.ok) throw new Error('Failed to start tuner');
      const j = await r.json();
      setTunerStatus(j.status || 'Tuner started!');
    } catch (e) {
      setTunerStatus('Error: ' + e.message);
    }
  };

  const stopTuner = async () => {
    try {
      await fetch('/api/tuner/stop', { method: 'POST' });
      setTunerStatus('Tuner stopped');
      setDetectedNote('');
      setCents(0);
    } catch (e) {
      setTunerStatus('Error stopping tuner');
    }
  };

  return (
    <div style={{ border: '2px solid #50C878', borderRadius: 8, padding: 16, margin: 8 }}>
      <h2>🎵 Universal Tuner</h2>
      <p>Real-time pitch detection and tuning</p>

      <section style={{ marginBottom: 12 }}>
        <label>Reference Note:</label>
        <div style={{ display: 'flex', gap: 8, marginTop: 4 }}>
          <select 
            value={note.slice(0, -1)} 
            onChange={e => setNote(e.target.value + note.slice(-1))}
            style={{ padding: 4 }}
          >
            {notes.map(n => <option key={n} value={n}>{n}</option>)}
          </select>
          <select 
            value={note.slice(-1)} 
            onChange={e => setNote(note.slice(0, -1) + e.target.value)}
            style={{ padding: 4 }}
          >
            {octaves.map(o => <option key={o} value={o}>{o}</option>)}
          </select>
        </div>
      </section>

      <section style={{ marginBottom: 12 }}>
        <label>Reference Frequency: {frequency} Hz</label>
        <input 
          type="range" 
          min="200" 
          max="800" 
          value={frequency} 
          onChange={e => setFrequency(+e.target.value)}
          style={{ width: '100%', marginTop: 4 }}
        />
      </section>

      {detectedNote && (
        <section style={{ marginBottom: 12, textAlign: 'center' }}>
          <div style={{ fontSize: '2em', fontWeight: 'bold' }}>
            {detectedNote}
          </div>
          <div style={{ 
            fontSize: '1.2em', 
            color: Math.abs(cents) < 10 ? '#50C878' : Math.abs(cents) < 25 ? '#FFA500' : '#FF4444'
          }}>
            {cents > 0 ? '+' : ''}{cents} cents
          </div>
          <div style={{ 
            width: '200px', 
            height: '20px', 
            backgroundColor: '#f0f0f0', 
            margin: '8px auto',
            position: 'relative',
            borderRadius: 10
          }}>
            <div style={{
              position: 'absolute',
              left: `${Math.max(0, Math.min(100, (cents + 50) * 2))}%`,
              top: 0,
              width: '4px',
              height: '100%',
              backgroundColor: '#333',
              borderRadius: 2
            }} />
          </div>
        </section>
      )}

      <div style={{ display: 'flex', gap: 8, marginTop: 16 }}>
        <button 
          onClick={startTuner}
          style={{ 
            padding: '8px 16px', 
            backgroundColor: '#50C878', 
            color: 'white', 
            border: 'none', 
            borderRadius: 4,
            cursor: 'pointer'
          }}
        >
          Start Tuner
        </button>
        <button 
          onClick={stopTuner}
          style={{ 
            padding: '8px 16px', 
            backgroundColor: '#FF4444', 
            color: 'white', 
            border: 'none', 
            borderRadius: 4,
            cursor: 'pointer'
          }}
        >
          Stop
        </button>
      </div>
      {tunerStatus && (
        <div style={{ marginTop: 8, opacity: 0.8 }}>{tunerStatus}</div>
      )}
    </div>
  );
}
