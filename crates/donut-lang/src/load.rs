use crate::types::common::{Error, SpanError};
use crate::types::env::Env;
use crate::types::token::Token;
use std::collections::HashMap;

fn span_to_pos_errors(span_errors: Vec<SpanError>, tokens: &[Token]) -> Vec<Error> {
    span_errors
        .into_iter()
        .filter_map(|(span, msg)| {
            let pos = tokens.get(span.start)?.pos.clone();
            Some((pos, msg))
        })
        .collect()
}

pub fn load(code: &str) -> (Env, Vec<Error>) {
    load_with_sources(code, HashMap::new())
}

pub fn load_with_sources(code: &str, extra_sources: HashMap<String, String>) -> (Env, Vec<Error>) {
    let mut errors: Vec<Error> = Vec::new();

    let (tokens, _, tok_errors) = crate::tokenize::tokenize(code);
    errors.extend(tok_errors);

    let (program, parse_errors) = crate::parse::parse(&tokens);
    errors.extend(parse_errors);

    let (sem_prog, conv_errors) = crate::convert::convert(program);
    errors.extend(span_to_pos_errors(conv_errors, &tokens));

    let (resolved, resolve_errors) =
        crate::resolve::resolve_with_sources(sem_prog, extra_sources);
    errors.extend(span_to_pos_errors(resolve_errors, &tokens));

    let (mut env, check_errors) = crate::check::check(&resolved);
    errors.extend(span_to_pos_errors(check_errors, &tokens));

    crate::deco::decorate(&mut env);

    (env, errors)
}
