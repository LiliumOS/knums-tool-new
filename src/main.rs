use std::{ffi::OsStr, io::ErrorKind, process::ExitCode};

use bincode::error::EncodeError;
use imt::{
    attr::{Attribute, types::ItemDoc},
    bundle::Bundle,
    config::format_config,
    header::Header,
    uuid::Uuid,
};
use logos::Logos;

use crate::{
    lexer::{Token, lex_file},
    parser::parse_file,
};

mod ast;
mod imt_builder;
mod lexer;
mod parser;

fn main() -> ExitCode {
    let mut args = std::env::args();

    let prg_name = args.next().unwrap();

    match real_main(&prg_name, args) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{prg_name}: {e}");
            ExitCode::FAILURE
        }
    }
}

fn real_main(prg_name: &str, mut args: impl Iterator<Item = String>) -> std::io::Result<()> {
    let mut input_file: Option<String> = None;
    let mut output_file: Option<String> = None;
    let mut prefix: Option<String> = None;
    let mut bundle = false;
    while let Some(arg) = args.next() {
        match &*arg {
            "--help" => {
                println!("Usage: {prg_name} [OPTIONS...] [--] input [output]");
                println!("Compiles knum files to IMT");
                println!("If no output file is provided, write directly to stdout");
                println!("Options:");
                println!(
                    "\t--bundle: Compile a bundle and write to a tar (instead of a direct .imt file)"
                );
                println!(
                    "\t--prefix <prefix>: Assumes all files begin with <prefix> (must be a valid path)"
                );
                println!("\t--help: Prints this message and exits");
                println!("\t--version: Prints version information and exits");
                return Ok(());
            }
            "--version" => {
                println!("knums-compiler v{}", core::env!("CARGO_PKG_VERSION"));
                return Ok(());
            }
            "--" => {
                input_file = args.next();
                break;
            }
            "--prefix" => {
                prefix = Some(args.next().ok_or_else(|| {
                    std::io::Error::new(ErrorKind::InvalidInput, "--prefix requires an argument")
                })?);
            }
            "--bundle" => {
                bundle = true;
            }
            x if x.starts_with("--") => {
                return Err(std::io::Error::new(
                    ErrorKind::InvalidInput,
                    format!("Unsupported option {x}"),
                ));
            }
            x => {
                input_file = Some(arg);
                break;
            }
        }
    }

    output_file = args.next();

    let input_file = input_file.ok_or_else(|| {
        std::io::Error::new(ErrorKind::InvalidInput, "input file must be provided")
    })?;

    let prefix = prefix
        .map(|f| f.split("::").map(String::from).collect::<Vec<_>>())
        .map(imt::bundle::Path)
        .unwrap_or_else(|| imt::bundle::Path(Vec::new()));

    if bundle {
        let mut bundle_file = Bundle::create();
        let output_file = output_file.ok_or_else(|| {
            std::io::Error::new(
                ErrorKind::InvalidInput,
                "--bundle requires there to be an output file",
            )
        })?;

        for walk in walkdir::WalkDir::new(&input_file) {
            let entry = walk?;
            let path = entry.path();
            if path.extension() != Some(OsStr::new("knum")) {
                continue;
            }
            let file = std::fs::File::open(path)?;
            let base = path.strip_prefix(&input_file).unwrap();

            let vpath = base
                .components()
                .map(|c| c.as_os_str().to_str().unwrap())
                .map(|c| c.strip_suffix(".knum").unwrap_or(c))
                .map(String::from)
                .collect::<Vec<_>>();

            let file = std::io::read_to_string(file)?;

            let file = convert_file(&file, path)?;
            bundle_file.add_file(imt::bundle::Path(vpath), file);
        }
        bundle_file.write_tar(&prefix, std::fs::File::create(output_file)?)?;
    } else {
        let file = std::fs::File::open(&input_file)?;
        let file = std::io::read_to_string(file)?;

        let file = convert_file(&file, &input_file)?;
        match if let Some(output_file) = output_file {
            bincode::encode_into_std_write(
                &file,
                &mut std::fs::File::create(output_file)?,
                format_config(),
            )
        } else {
            bincode::encode_into_std_write(&file, &mut std::io::stdout().lock(), format_config())
        } {
            Ok(_) => {}
            Err(EncodeError::Io { inner, .. }) => return Err(inner),
            Err(e) => return Err(std::io::Error::new(ErrorKind::InvalidData, e)),
        }
    }

    Ok(())
}

fn convert_file(
    file: &str,
    fname: impl AsRef<std::path::Path>,
) -> std::io::Result<imt::file::File> {
    let fname = fname.as_ref();
    let tokens = match lex_file(file) {
        Ok(tokens) => tokens,
        Err(e) => {
            return Err(std::io::Error::new(
                ErrorKind::InvalidData,
                format!("{}: {e}", fname.display()),
            ));
        }
    };

    eprintln!("{tokens:?}");

    let file = match parse_file(&tokens) {
        Ok(file) => file,
        Err(e) => {
            return Err(std::io::Error::new(
                ErrorKind::InvalidData,
                format!("{}: {e}", fname.display()),
            ));
        }
    };

    eprintln!("{file:?}");

    let mut imt_file = imt::file::File {
        header: Header::CURRENT,
        file_id: Uuid(uuid::Uuid::now_v7().into()),
        attributes: Vec::new(),
        uses: Vec::new(),
        types: Vec::new(),
        values: Vec::new(),
    };

    imt_file.attributes.push(Attribute::new(ItemDoc {
        doc_lines: file.file_doc.into_iter().map(|x| x.to_string()).collect(),
    }));

    match file
        .items
        .into_iter()
        .map(|v| imt_builder::convert_item(&mut imt_file, v))
        .collect::<Result<(), _>>()
    {
        Ok(()) => Ok(imt_file),
        Err(e) => Err(std::io::Error::new(
            ErrorKind::InvalidData,
            format!("{}: {e:?}", fname.display()),
        )),
    }
}
