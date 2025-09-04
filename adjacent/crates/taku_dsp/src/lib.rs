/// Minimal bitcrusher core (placeholder math).
/// Later, add anti-alias options, interpolation, and vectorization.
#[derive(Clone, Copy, Debug)]
pub struct Bitcrusher {
    pub bits: u8,       // 1..=24
    pub rate_hz: u32,   // 100..=44100
}

impl Default for Bitcrusher {
    fn default() -> Self { Self { bits: 12, rate_hz: 16_000 } }
}

impl Bitcrusher {
    pub fn process_sample(&self, x: f32, sr: f32, phase: &mut f32) -> f32 {
        // Downsample by holding the last value for N input samples.
        let hold_n = (sr / self.rate_hz.max(1) as f32).max(1.0);
        *phase += 1.0;
        if *phase >= hold_n { *phase = 0.0; }

        // Quantize to 2^bits levels (very naive placeholder)
        let levels = (1u32 << self.bits.min(24) as u32) as f32;
        let y = (x * levels).round() / levels;
        y
    }

    pub fn process_buffer_inplace(&self, buf: &mut [f32], sr: f32) {
        let mut phase = 0.0f32;
        for s in buf.iter_mut() {
            *s = self.process_sample(*s, sr, &mut phase);
        }
    }
}
