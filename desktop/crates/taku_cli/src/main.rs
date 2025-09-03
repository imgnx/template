use clap::Parser;
use taku_dsp::Bitcrusher;

#[derive(Parser, Debug)]
#[command(name = "taku-cli", version, about = "Taku Bitcrusher CLI (stub)")]
struct Args {
    /// Bit depth (1..24)
    #[arg(long, default_value_t = 12)]
    bits: u8,
    /// Sample rate reduction in Hz (100..44100)
    #[arg(long, default_value_t = 16_000)]
    rate: u32,
}

fn main() {
    let args = Args::parse();
    let dsp = Bitcrusher { bits: args.bits, rate_hz: args.rate };
    println!("CLI up. bits={}, rate_hz={}", dsp.bits, dsp.rate_hz);
    // TODO: read WAV from stdin/file, process via dsp.process_buffer_inplace, write WAV.
}
