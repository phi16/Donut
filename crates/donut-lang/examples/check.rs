use std::process;

use donut_core::cell::Globular;
use donut_core::common::PureVal;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let mut show_resolve = false;
    let mut files = Vec::new();

    for arg in &args[1..] {
        match arg.as_str() {
            "--resolve" | "-r" => show_resolve = true,
            "--help" | "-h" => {
                eprintln!("Usage: check [OPTIONS] <file.donut>...");
                eprintln!();
                eprintln!("Options:");
                eprintln!("  -r, --resolve   Show resolve output");
                eprintln!("  -h, --help      Show this help");
                process::exit(0);
            }
            _ => files.push(arg.clone()),
        }
    }

    if files.is_empty() {
        eprintln!("Usage: check [OPTIONS] <file.donut>...");
        process::exit(1);
    }

    let mut has_error = false;

    for file in &files {
        let code = match std::fs::read_to_string(file) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("{}: {}", file, e);
                has_error = true;
                continue;
            }
        };

        if files.len() > 1 {
            eprintln!("=== {} ===", file);
        }

        let code = donut_core::common::dedent(&code);

        // Tokenize → Parse → Convert → Resolve
        let (tokens, _, tok_errors) = donut_lang::tokenize::tokenize(&code);
        let (program, parse_errors) = donut_lang::parse::parse(&tokens);
        let (sem_prog, conv_errors) = donut_lang::convert::convert(program, &tokens);
        let (resolved, resolve_errors) = donut_lang::resolve::resolve(sem_prog, &tokens);

        // Show resolve output if requested
        if show_resolve {
            eprintln!("{}", resolved.display_all());
        }

        // Check
        let (env, check_errors) = donut_lang::check::check(&resolved, &tokens);

        // Collect all errors
        let mut errors = Vec::new();
        for (pos, msg) in tok_errors
            .into_iter()
            .chain(parse_errors)
            .chain(conv_errors)
            .chain(resolve_errors)
            .chain(check_errors)
        {
            errors.push(format!("{}:{}: {}", pos.line + 1, pos.col + 1, msg));
        }

        if !errors.is_empty() {
            for e in &errors {
                eprintln!("{}", e);
            }
            has_error = true;
            continue;
        }

        // Print definitions
        for def in &env.defs {
            if def.origin.is_some() {
                continue; // skip imported defs
            }
            let sig = env.display_def_signature(def);
            let dim_info = match &def.val {
                PureVal::Cell(pc) => {
                    format!("  ({}d)", pc.dim().in_space)
                }
                _ => String::new(),
            };
            println!("{}{}", sig, dim_info);
        }
    }

    if has_error {
        process::exit(1);
    }
}
