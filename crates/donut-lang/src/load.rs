use crate::check::Env;
use crate::types::common::Error;

pub fn load(code: &str) -> (Env, Vec<Error>) {
    let mut errors: Vec<Error> = Vec::new();

    let (tokens, _, tok_errors) = crate::tokenize::tokenize(&code);
    errors.extend(tok_errors);

    let (program, parse_errors) = crate::parse::parse(&tokens);
    errors.extend(parse_errors);

    let (sem_prog, conv_errors) = crate::convert::convert(program, &tokens);
    errors.extend(conv_errors);

    let (resolved, resolve_errors) = crate::resolve::resolve(sem_prog, &tokens);
    errors.extend(resolve_errors);

    let (env, check_errors) = crate::check::check(&resolved, &tokens);
    errors.extend(check_errors);

    (env, errors)
}
