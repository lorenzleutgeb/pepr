use clap::{AppSettings, Parser, Subcommand};
use clap_verbosity_flag::Verbosity;
use std::fmt::Write;
use std::path::PathBuf;
use std::{fmt::format, mem::size_of};

use crate::symbols::{Constant, Integer, Term, Variable};

#[derive(Parser)]
#[clap(author, version, about, long_about = None, long_version = version())]
pub struct Cli {
    #[clap(flatten)]
    pub verbosity: Verbosity,

    #[clap(short, long, parse(from_os_str), value_name = "FILE")]
    pub input: PathBuf,
}

fn version() -> &'static str {
    Box::leak(Box::new(format!(
        "{}
Sizes in bytes:
  Term               {}
  Variable           {}
  Symbolic Constant  {}
  Integer  Constant  {} (min. {}, max. {})",
        env!("CARGO_PKG_VERSION"),
        size_of::<Term>(),
        size_of::<Variable>(),
        size_of::<Constant>(),
        size_of::<Integer>(),
        Integer::MIN,
        Integer::MAX
    )))
}
