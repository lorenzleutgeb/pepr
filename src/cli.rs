use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
pub struct Cli {
    /// Optional name to operate on
    pub name: Option<String>,

    /// Sets a custom config file
    #[clap(short, long, parse(from_os_str), value_name = "FILE")]
    pub config: Option<PathBuf>,

    /// Turn debugging information on
    #[clap(short, long, parse(from_occurrences))]
    pub debug: usize,

    #[clap(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Runs saturation prover
    Horn {
        /// Sets a custom config file
        #[clap(short, long, parse(from_os_str), value_name = "FILE")]
        input: PathBuf,
    },
    /// hammers the input to specified output format.
    Hammer {},
}
