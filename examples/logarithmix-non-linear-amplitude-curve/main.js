// Panama-Canal Lift: tuned comb (with damping) + twin peaks around f0.
// Works in Web Audio. Minimal, production-ready skeleton.

class PanamaCanalLift {
  constructor(audioCtx, { sampleRate = 44100 } = {}) {
    this.ac = audioCtx;
    this.sampleRate = sampleRate;

    // I/O
    this.input = this.ac.createGain();
    this.output = this.ac.createGain();

    // --- Tuned comb with damping in the feedback loop ---
    this.combDelay = this.ac.createDelay(1.0); // up to 1s delay
    this.combFB = this.ac.createGain(); // feedback gain
    this.combDamp = this.ac.createBiquadFilter(); // LPF in feedback
    this.combDamp.type = "lowpass";
    this.combDamp.frequency.value = 4000; // damping cutoff
    this.combFB.gain.value = 0.4; // 0.25–0.6 typical

    // feedback wiring: input -> delay -> damp -> FB gain -> back to delay
    this.input.connect(this.combDelay);
    this.combDelay.connect(this.combDamp);
    this.combDamp.connect(this.combFB);
    this.combFB.connect(this.combDelay); // close the loop

    // tap comb output
    this.combOut = this.ac.createGain();
    this.combDelay.connect(this.combOut);

    // --- Twin-peak "canal walls" ---
    this.split = this.ac.createChannelSplitter(2);
    this.input.connect(this.split);

    // two peaking filters slightly off the note
    this.peakLow = this.ac.createBiquadFilter();
    this.peakHigh = this.ac.createBiquadFilter();
    this.peakLow.type = "peaking";
    this.peakHigh.type = "peaking";
    this.peakLow.Q.value = 12; // 8–20
    this.peakHigh.Q.value = 12;
    this.peakLow.gain.value = 4; // +3 to +6 dB
    this.peakHigh.gain.value = 4;

    // feed both with the original (mono-safe)
    this.input.connect(this.peakLow);
    this.input.connect(this.peakHigh);

    // --- Mixers ---
    this.combMix = this.ac.createGain();
    this.combMix.gain.value = 0.25; // 10–35%
    this.wallsMix = this.ac.createGain();
    this.wallsMix.gain.value = 0.15;

    this.combOut.connect(this.combMix);
    this.peakLow.connect(this.wallsMix);
    this.peakHigh.connect(this.wallsMix);

    // Dry path
    this.dry = this.ac.createGain();
    this.dry.gain.value = 1.0;
    this.input.connect(this.dry);

    // Sum
    this.sum = this.ac.createGain();
    this.dry.connect(this.sum);
    this.combMix.connect(this.sum);
    this.wallsMix.connect(this.sum);
    this.sum.connect(this.output);

    // Optional: subtle LFO to breathe the comb delay by ±1–3 cents at golden timing
    this.lfo = this.ac.createOscillator();
    this.lfo.frequency.value = 0.0; // set with setGoldenLFO(bpm)
    this.lfoGain = this.ac.createGain();
    this.lfoGain.gain.value = 0.00002; // tiny seconds modulation
    this.lfo.connect(this.lfoGain);
    this.lfoGain.connect(this.combDelay.delayTime);
    this.lfo.start();
  }

  // Utility: cents -> ratio
  static centsToRatio(cents) {
    return Math.pow(2, cents / 1200);
  }

  // Configure from detected f0 (Hz)
  setFromHz(f0) {
    // Comb delay time = 1/f0 (clamped to DelayNode max)
    const d = 1 / Math.max(1, f0);
    this.combDelay.delayTime.setTargetAtTime(d, this.ac.currentTime, 0.01);

    // Twin peaks at ±Δ cents around f0
    const deltaCents = 18; // try 10–25
    const fLow = f0 / PanamaCanalLift.centsToRatio(deltaCents);
    const fHigh = f0 * PanamaCanalLift.centsToRatio(deltaCents);
    this.peakLow.frequency.setTargetAtTime(fLow, this.ac.currentTime, 0.01);
    this.peakHigh.frequency.setTargetAtTime(fHigh, this.ac.currentTime, 0.01);

    // (Optional) make wall gains track your note envelope externally for "buoyancy"
    // e.g., this.peakLow.gain.value = base + env * depth; same for peakHigh.
  }

  setCombFeedback(amount) {
    // 0..1
    this.combFB.gain.value = Math.max(0, Math.min(0.95, amount));
  }

  setCombDamping(cutoffHz) {
    this.combDamp.frequency.value = Math.max(200, cutoffHz | 0);
  }

  setMixes({ dry = 1.0, comb = 0.25, walls = 0.15 } = {}) {
    this.dry.gain.value = dry;
    this.combMix.gain.value = comb;
    this.wallsMix.gain.value = walls;
  }

  // Golden-ratio LFO rate: very slow wobble synced to 0.618 beats
  setGoldenLFO(bpm) {
    const PHI = (1 + Math.sqrt(5)) / 2;
    const beatSec = 60 / bpm;
    const t = beatSec / PHI; // 0.618 of a beat
    this.lfo.frequency.value = 1 / (t * 8); // very slow; divide further if needed
  }
}

// Example wiring:
// const ac = new AudioContext();
// const canal = new PanamaCanalLift(ac);
// someInputNode.connect(canal.input);
// canal.output.connect(ac.destination);
// canal.setFromHz(220); // A3
// canal.setMixes({ dry: 1, comb: 0.22, walls: 0.12 });
// canal.setGoldenLFO(120);
