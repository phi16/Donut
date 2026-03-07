use crate::check::Env;
use crate::types::common::Error;
use std::collections::HashMap;

pub fn load(code: &str) -> (Env, Vec<Error>) {
    load_with_sources(code, HashMap::new())
}

pub fn load_with_sources(code: &str, extra_sources: HashMap<String, String>) -> (Env, Vec<Error>) {
    let mut errors: Vec<Error> = Vec::new();

    let (tokens, _, tok_errors) = crate::tokenize::tokenize(&code);
    errors.extend(tok_errors);

    let (program, parse_errors) = crate::parse::parse(&tokens);
    errors.extend(parse_errors);

    let (sem_prog, conv_errors) = crate::convert::convert(program, &tokens);
    errors.extend(conv_errors);

    let (resolved, resolve_errors) = crate::resolve::resolve_with_sources(sem_prog, &tokens, extra_sources);
    errors.extend(resolve_errors);

    let (env, check_errors) = crate::check::check(&resolved, &tokens);
    errors.extend(check_errors);

    (env, errors)
}
