// Parser implementation created by Claude Code
// https://claude.ai/code

use crate::types::*;
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, digit1, multispace0, multispace1, space0},
    combinator::{map, opt},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated},
};

/*

decl = name (name* | '[' arg-decls ']') (':' cell-type)? ('=' cell)?
     | '[' decorator % ',' ']' decl
arg-decls = (name+ (':' arg-type)?) % ','
cell-type = '*'
          | cell '→' cell
lit-type = 'nat' | 'int' | 'rat' | 'str'
lit = nat | int | rat | str
arg-type = cell-type
         | lit-type
arg = cell
    | lit
cell = name ('[' (arg % ',') ']')?
     | cell (';'* | ';' nat ';') cell
     | '(' cell ')'
decorator = name ('(' (arg % ',') ')')?
program = decl*

*/

// Helper: skip whitespace
fn ws<'a, F, O>(mut inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
{
    move |input| {
        let (input, _) = multispace0(input)?;
        let (input, res) = inner.parse(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, res))
    }
}

// name (identifier) - skips all whitespace including newlines
fn ident(input: &str) -> IResult<&str, Ident> {
    let (input, _) = multispace0(input)?;
    let (input, name) =
        take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '-' || c == '.')(input)?;
    // Identifiers must start with alphabetic or underscore
    if !name.chars().next().unwrap().is_alphabetic() && name.chars().next().unwrap() != '_' {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alpha,
        )));
    }
    let (input, _) = multispace0(input)?;
    Ok((input, name.to_string()))
}

// name (identifier) for cell parsing - only skips horizontal whitespace, not newlines
fn ident_for_cell(input: &str) -> IResult<&str, Ident> {
    let (input, _) = space0(input)?;
    let (input, name) =
        take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '-' || c == '.')(input)?;
    // Identifiers must start with alphabetic or underscore
    if !name.chars().next().unwrap().is_alphabetic() && name.chars().next().unwrap() != '_' {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alpha,
        )));
    }
    let (input, _) = space0(input)?;
    Ok((input, name.to_string()))
}

// nat literal
fn nat_lit(input: &str) -> IResult<&str, Lit> {
    let (input, _) = multispace0(input)?;
    let (input, n) = digit1(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, Lit::Nat(n.parse().unwrap())))
}

// int literal (signed) - only matches if there's actually a minus sign
fn int_lit(input: &str) -> IResult<&str, Lit> {
    let (input, _) = multispace0(input)?;
    let (input, _) = char('-')(input)?;
    let (input, n) = digit1(input)?;
    let (input, _) = multispace0(input)?;
    let val = format!("-{}", n).parse().unwrap();
    Ok((input, Lit::Int(val)))
}

// rat literal (float)
fn rat_lit(input: &str) -> IResult<&str, Lit> {
    let (input, _) = multispace0(input)?;
    let (input, sign) = opt(char('-')).parse(input)?;
    let (input, int_part) = digit1(input)?;
    let (input, _) = char('.')(input)?;
    let (input, frac_part) = digit1(input)?;
    let (input, _) = multispace0(input)?;
    let s = if sign.is_some() {
        format!("-{}.{}", int_part, frac_part)
    } else {
        format!("{}.{}", int_part, frac_part)
    };
    Ok((input, Lit::Rat(s.parse().unwrap())))
}

// str literal
fn str_lit(input: &str) -> IResult<&str, Lit> {
    let (input, _) = multispace0(input)?;
    let (input, _) = char('"')(input)?;
    let (input, s) = take_while(|c| c != '"')(input)?;
    let (input, _) = char('"')(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, Lit::Str(s.to_string())))
}

// lit
fn lit(input: &str) -> IResult<&str, Lit> {
    alt((rat_lit, int_lit, nat_lit, str_lit)).parse(input)
}

// lit-type
fn lit_type(input: &str) -> IResult<&str, LitType> {
    let (input, _) = multispace0(input)?;
    let (input, name) = alt((tag("nat"), tag("int"), tag("rat"), tag("str"))).parse(input)?;
    let (input, _) = multispace0(input)?;
    let ty = match name {
        "nat" => LitType::Nat,
        "int" => LitType::Int,
        "rat" => LitType::Rat,
        "str" => LitType::Str,
        _ => unreachable!(),
    };
    Ok((input, ty))
}

// cell (primary)
fn cell_primary(input: &str) -> IResult<&str, Cell> {
    alt((
        // '(' cell ')'
        delimited(ws(char('(')), cell, ws(char(')'))),
        // name ('[' (arg % ',') ']')?
        map(
            pair(
                ident_for_cell,
                opt(delimited(
                    ws(char('[')),
                    separated_list0(ws(char(',')), arg),
                    preceded(multispace0, char(']')), // ']' の後の空白はスキップしない
                )),
            ),
            |(name, args)| Cell::Var(name, args.unwrap_or_default()),
        ),
    ))
    .parse(input)
}

// cell (with composition) - with operator precedence
// Lower axis number = higher precedence (binds tighter)
fn cell(input: &str) -> IResult<&str, Cell> {
    cell_with_precedence(input, 255)
}

// Parse semicolon operator (returns axis number)
fn semicolon_operator(input: &str) -> IResult<&str, u8> {
    // Skip leading horizontal whitespace only (not newlines)
    let (input, _) = space0(input)?;

    let (input, axis) = alt((
        // ;N; format (axis with explicit number)
        map(
            delimited(
                char::<&str, nom::error::Error<&str>>(';'),
                digit1,
                char(';'),
            ),
            |n: &str| n.parse::<u8>().unwrap(),
        ),
        // ;;; format (count semicolons)
        map(many1(char(';')), |semis| semis.len() as u8),
    ))
    .parse(input)?;

    // Skip trailing whitespace including newlines
    // This allows multi-line expressions when using semicolons
    let (input, _) = multispace0(input)?;
    Ok((input, axis))
}

fn cell_with_precedence(input: &str, min_precedence: u8) -> IResult<&str, Cell> {
    let (input, left) = cell_primary(input)?;
    cell_with_precedence_inner(input, left, min_precedence)
}

fn cell_with_precedence_inner(
    mut input: &str,
    mut left: Cell,
    min_precedence: u8,
) -> IResult<&str, Cell> {
    loop {
        // Save the input position before trying operators
        let before_whitespace = input;

        // Try to parse a semicolon-based operator first
        let axis = match semicolon_operator(input) {
            Ok((rest, axis)) => {
                input = rest;
                Some(axis)
            }
            Err(_) => {
                // No semicolon operator
                // Check if there's another cell_primary following (space-separated, no newline)
                // Since ident skips trailing whitespace (including newlines), we need to check
                // if there's a newline before the next token
                // If current position starts with newline, don't treat as space-separated
                if input.starts_with('\n') || input.starts_with("\r\n") {
                    None
                } else {
                    // Try to parse another cell_primary
                    match cell_primary(input) {
                        Ok(_) => {
                            // Successfully can parse next primary
                            // This means there's a space-separated composition (axis 0)
                            Some(0)
                        }
                        Err(_) => None,
                    }
                }
            }
        };

        let axis = match axis {
            Some(a) => a,
            None => {
                // No operator found
                input = before_whitespace;
                break;
            }
        };

        // Check if this operator has high enough precedence
        // Lower axis number = higher precedence = lower min_precedence value needed
        if axis >= min_precedence {
            // Precedence too low, restore position and stop here
            input = before_whitespace;
            break;
        }

        // Parse the right side
        // Using axis+1 gives right-associativity (groups tighter on right)
        let next_min_prec = axis + 1;
        let (rest, right) = cell_with_precedence(input, next_min_prec)?;
        input = rest;
        left = Cell::Comp(axis, vec![left, right]);
    }

    Ok((input, left))
}

// arg
fn arg(input: &str) -> IResult<&str, Arg> {
    alt((map(lit, Arg::Lit), map(cell, Arg::Cell))).parse(input)
}

// arg-type
fn arg_type(input: &str) -> IResult<&str, ArgType> {
    alt((map(lit_type, ArgType::Lit), map(cell_type, ArgType::Cell))).parse(input)
}

// cell-type
fn cell_type(input: &str) -> IResult<&str, CellType> {
    alt((
        map(ws(char('*')), |_| CellType::Star),
        map((cell, ws(tag("→")), cell), |(left, _, right)| {
            CellType::Arr(left, right)
        }),
    ))
    .parse(input)
}

// decorator
fn decorator(input: &str) -> IResult<&str, Decorator> {
    let (input, name) = ident(input)?;
    let (input, args) = opt(delimited(
        ws(char('(')),
        separated_list0(ws(char(',')), arg),
        ws(char(')')),
    ))
    .parse(input)?;
    Ok((
        input,
        Decorator {
            name,
            args: args.unwrap_or_default(),
        },
    ))
}

// ident without trailing whitespace skip (for use in separated lists)
fn ident_raw(input: &str) -> IResult<&str, Ident> {
    let (input, name) =
        take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '-' || c == '.')(input)?;
    // Identifiers must start with alphabetic or underscore
    if !name.chars().next().unwrap().is_alphabetic() && name.chars().next().unwrap() != '_' {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alpha,
        )));
    }
    Ok((input, name.to_string()))
}

// arg-decls
fn arg_decl(input: &str) -> IResult<&str, ArgDecl> {
    let (input, names) = separated_list1(multispace1, ident_raw).parse(input)?;
    let (input, _) = multispace0(input)?;
    let (input, ty) = opt(preceded(ws(char(':')), arg_type)).parse(input)?;
    Ok((input, ArgDecl { names, ty }))
}

fn arg_decls(input: &str) -> IResult<&str, Vec<ArgDecl>> {
    separated_list0(ws(char(',')), arg_decl).parse(input)
}

// Parse ident, only skipping horizontal whitespace (not newlines)
fn ident_no_newline(input: &str) -> IResult<&str, Ident> {
    let (input, _) = space0(input)?;
    let (input, name) =
        take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '-' || c == '.')(input)?;
    // Identifiers must start with alphabetic or underscore
    if !name.chars().next().unwrap().is_alphabetic() && name.chars().next().unwrap() != '_' {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alpha,
        )));
    }
    let (input, _) = space0(input)?;
    Ok((input, name.to_string()))
}

// Parse names until we see '[', ':', '=' or newline
fn names_until_special(input: &str) -> IResult<&str, Vec<Ident>> {
    let (mut input, first) = ident_no_newline(input)?;
    let mut names = vec![first];

    loop {
        // Check if there's a newline before next token
        if input.starts_with('\n') || input.starts_with("\r\n") {
            break;
        }

        // Check if next token (after horizontal whitespace) is '[', ':', or '='
        match space0::<&str, nom::error::Error<&str>>(input) {
            Ok((rest, _)) => {
                if rest.starts_with('[') || rest.starts_with(':') || rest.starts_with('=') {
                    break;
                }
                if rest.starts_with('\n') || rest.starts_with("\r\n") {
                    break;
                }
                // Try to parse another ident (without skipping newlines)
                // We need to parse just the identifier without multispace0
                match take_while1::<_, &str, nom::error::Error<&str>>(|c: char| {
                    c.is_alphanumeric() || c == '_' || c == '-' || c == '.'
                })(rest)
                {
                    Ok((rest2, name)) => {
                        // Check if starts with alphabetic or underscore
                        if name.chars().next().unwrap().is_alphabetic()
                            || name.chars().next().unwrap() == '_'
                        {
                            input = rest2;
                            names.push(name.to_string());
                        } else {
                            break;
                        }
                    }
                    Err(_) => break,
                }
            }
            Err(_) => break,
        }
    }

    Ok((input, names))
}

// Simple declaration parser (without decorators)
fn simple_decl(input: &str) -> IResult<&str, Decl> {
    let (mut input, names) = names_until_special(input)?;

    // Skip horizontal whitespace only (not newlines)
    let (rest, _) = space0(input)?;
    input = rest;

    // Check for brackets (only if we have exactly one name)
    let (mut input, args) = if names.len() == 1 && input.starts_with('[') {
        let (input, args) = delimited(
            char('['),
            preceded(space0, arg_decls),
            preceded(space0, char(']')),
        )
        .parse(input)?;
        (input, args)
    } else {
        (input, vec![])
    };

    // Skip horizontal whitespace
    let (rest, _) = space0(input)?;
    input = rest;

    // Check for type annotation
    let (mut input, ty) = if input.starts_with(':') {
        let (input, ty) = preceded(char(':'), preceded(space0, cell_type)).parse(input)?;
        (input, Some(ty))
    } else {
        (input, None)
    };

    // Skip horizontal whitespace
    let (rest, _) = space0(input)?;
    input = rest;

    // Check for body
    let (input, body) = if input.starts_with('=') {
        // Use multispace0 to allow body to start on the next line
        let (input, body) = preceded(char('='), preceded(multispace0, cell)).parse(input)?;
        (input, Some(body))
    } else {
        (input, None)
    };

    Ok((
        input,
        Decl {
            decos: vec![],
            names,
            args,
            ty,
            body,
        },
    ))
}

// decl
fn decl(input: &str) -> IResult<&str, Decl> {
    // '[' decorator % ',' ']' decl
    let with_decorators = map(
        (
            delimited(
                ws(char('[')),
                separated_list1(ws(char(',')), decorator),
                ws(char(']')),
            ),
            decl,
        ),
        |(mut decos, mut d)| {
            decos.append(&mut d.decos);
            d.decos = decos;
            d
        },
    );

    alt((with_decorators, simple_decl)).parse(input)
}

// program
fn program(input: &str) -> IResult<&str, Vec<Decl>> {
    let (input, _) = multispace0(input)?;
    let (input, decls) = many0(terminated(decl, multispace0)).parse(input)?;
    Ok((input, decls))
}

pub fn parse_program(input: &str) -> std::result::Result<Vec<Decl>, String> {
    let result = program(input).map_err(|e| e.to_string())?;
    if !result.0.is_empty() {
        return Err(format!("input remaining: {}", result.0));
    }
    Ok(result.1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_ident() {
        let result = ident("foo");
        assert!(result.is_ok());
        let (rest, name) = result.unwrap();
        assert_eq!(rest, "");
        assert_eq!(name, "foo");
    }

    #[test]
    fn test_ident_with_dash() {
        let result = ident("foo-bar");
        assert!(result.is_ok());
        let (_, name) = result.unwrap();
        assert_eq!(name, "foo-bar");
    }

    #[test]
    fn test_nat_literal() {
        let result = nat_lit("123");
        assert!(result.is_ok());
        let (_, lit) = result.unwrap();
        assert!(matches!(lit, Lit::Nat(123)));
    }

    #[test]
    fn test_int_literal() {
        let result = int_lit("-42");
        assert!(result.is_ok());
        let (_, lit) = result.unwrap();
        assert!(matches!(lit, Lit::Int(-42)));
    }

    #[test]
    fn test_rat_literal() {
        let result = rat_lit("3.14");
        assert!(result.is_ok());
        let (_, lit) = result.unwrap();
        if let Lit::Rat(val) = lit {
            assert!((val - 3.14).abs() < 0.001);
        } else {
            panic!("Expected Rat literal");
        }
    }

    #[test]
    fn test_str_literal() {
        let result = str_lit("\"hello world\"");
        assert!(result.is_ok());
        let (_, lit) = result.unwrap();
        assert!(matches!(lit, Lit::Str(s) if s == "hello world"));
    }

    #[test]
    fn test_simple_cell() {
        let result = cell("foo");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        assert_eq!(c, Cell::Var("foo".to_string(), vec![]));
    }

    #[test]
    fn test_cell_with_args() {
        let result = cell("foo[1, 2]");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        assert_eq!(
            c,
            Cell::Var(
                "foo".to_string(),
                vec![Arg::Lit(Lit::Nat(1)), Arg::Lit(Lit::Nat(2)),]
            )
        );
    }

    #[test]
    fn test_cell_composition_simple() {
        let result = cell("a ; b");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        assert_eq!(
            c,
            Cell::Comp(
                1,
                vec![
                    Cell::Var("a".to_string(), vec![]),
                    Cell::Var("b".to_string(), vec![]),
                ]
            )
        );
    }

    #[test]
    fn test_cell_composition_multiple_semicolons() {
        let result = cell("a ;; b");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        assert!(matches!(c, Cell::Comp(2, _)));
    }

    #[test]
    fn test_cell_composition_with_axis() {
        let result = cell("a ;3; b");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        assert!(matches!(c, Cell::Comp(3, _)));
    }

    #[test]
    fn test_cell_composition_complex() {
        let result = cell("a ; b ;; c");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        // Should be: Comp(1, [a, b]) ; Comp(2, [..., c])
        // Due to left-to-right folding
        if let Cell::Comp(axis, cells) = c {
            assert_eq!(axis, 2);
            assert_eq!(cells.len(), 2);
        } else {
            panic!("Expected Cell::Comp");
        }
    }

    #[test]
    fn test_cell_with_parens() {
        let result = cell("(foo)");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        assert!(matches!(c, Cell::Var(name, _) if name == "foo"));
    }

    #[test]
    fn test_simple_decl() {
        let result = decl("foo x y = bar");
        assert!(result.is_ok());
        let (_, d) = result.unwrap();
        assert_eq!(d.names, vec!["foo", "x", "y"]);
        assert!(d.body.is_some());
    }

    #[test]
    fn test_arg_decls_simple() {
        let result = arg_decls("x y: nat, z: int");
        assert!(result.is_ok());
        let (_, args) = result.unwrap();
        assert_eq!(args.len(), 2);
    }

    #[test]
    fn test_decl_with_brackets() {
        let result = decl("foo [x y: nat, z: int] = bar");
        assert!(result.is_ok());
        let (_, d) = result.unwrap();
        assert_eq!(d.names, vec!["foo"]);
        assert_eq!(d.args.len(), 2);
        assert_eq!(d.args[0].names, vec!["x", "y"]);
        assert_eq!(d.args[1].names, vec!["z"]);
    }

    #[test]
    fn test_decl_with_type() {
        let result = decl("foo : * = bar");
        assert!(result.is_ok());
        let (_, d) = result.unwrap();
        assert!(d.ty.is_some());
        assert!(matches!(d.ty.unwrap(), CellType::Star));
    }

    #[test]
    fn test_decl_with_decorator() {
        let result = decl("[inline] foo = bar");
        assert!(result.is_ok());
        let (_, d) = result.unwrap();
        assert_eq!(d.decos.len(), 1);
        assert_eq!(d.decos[0].name, "inline");
    }

    #[test]
    fn test_decl_with_multiple_decorators() {
        let result = decl("[inline, pure] foo = bar");
        assert!(result.is_ok());
        let (_, d) = result.unwrap();
        assert_eq!(d.decos.len(), 2);
        assert_eq!(d.decos[0].name, "inline");
        assert_eq!(d.decos[1].name, "pure");
    }

    #[test]
    fn test_decl_with_multiple_decorator_brackets() {
        let result = decl("[inline] [pure] foo = bar");
        assert!(result.is_ok());
        let (_, d) = result.unwrap();
        assert_eq!(d.decos.len(), 2);
        assert_eq!(d.decos[0].name, "inline");
        assert_eq!(d.decos[1].name, "pure");
    }

    #[test]
    fn test_decorator_with_args() {
        let result = decl("[optimize(3)] foo = bar");
        assert!(result.is_ok());
        let (_, d) = result.unwrap();
        assert_eq!(d.decos.len(), 1);
        assert_eq!(d.decos[0].name, "optimize");
        assert_eq!(d.decos[0].args.len(), 1);
    }

    #[test]
    fn test_program_single_decl() {
        let result = program("foo = bar");
        assert!(result.is_ok());
        let (_, decls) = result.unwrap();
        assert_eq!(decls.len(), 1);
    }

    #[test]
    fn test_program_multiple_decls() {
        let result = program("foo = bar\nbaz = qux");
        assert!(result.is_ok());
        let (_, decls) = result.unwrap();
        assert_eq!(decls.len(), 2);
    }

    #[test]
    fn test_complex_program() {
        let input = r#"
            [inline]
            [pure]
            identity [x: *] : * = x

            compose [f g] = f[g]

            map-2d f arr = row-map[f] ; col-map[f] ; arr
        "#;

        let result = program(input);
        assert!(result.is_ok());
        let (_, decls) = result.unwrap();

        eprintln!("Decls count: {}", decls.len());
        for (i, d) in decls.iter().enumerate() {
            eprintln!("decl[{}]: names={:?}", i, d.names);
        }

        assert_eq!(decls.len(), 3);

        // Check first declaration
        assert_eq!(decls[0].decos.len(), 2);
        assert_eq!(decls[0].names[0], "identity");

        // Check second declaration
        assert_eq!(decls[1].names[0], "compose");

        // Check third declaration
        assert_eq!(decls[2].names[0], "map-2d");
    }

    #[test]
    fn test_nested_composition() {
        let result = cell("(a ; b) ;; (c ; d)");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        if let Cell::Comp(axis, cells) = c {
            assert_eq!(axis, 2);
            assert_eq!(cells.len(), 2);
            assert!(matches!(cells[0], Cell::Comp(1, _)));
            assert!(matches!(cells[1], Cell::Comp(1, _)));
        } else {
            panic!("Expected Cell::Comp");
        }
    }

    #[test]
    fn test_cell_with_complex_args() {
        let result = cell("foo[a ; b, \"hello\", 42]");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        if let Cell::Var(name, args) = c {
            assert_eq!(name, "foo");
            assert_eq!(args.len(), 3);
            assert!(matches!(args[0], Arg::Cell(_)));
            assert!(matches!(args[1], Arg::Lit(Lit::Str(_))));
            assert!(matches!(args[2], Arg::Lit(Lit::Nat(42))));
        } else {
            panic!("Expected Cell::Var");
        }
    }

    #[test]
    fn test_arrow_type() {
        let result = cell_type("a → b");
        assert!(result.is_ok());
        let (_, ty) = result.unwrap();
        assert!(matches!(ty, CellType::Arr(_, _)));
    }

    #[test]
    fn test_empty_program() {
        let result = program("");
        assert!(result.is_ok());
        let (_, decls) = result.unwrap();
        assert_eq!(decls.len(), 0);
    }

    #[test]
    fn test_decl_no_body() {
        let result = decl("foo x y : *");
        assert!(result.is_ok());
        let (_, d) = result.unwrap();
        assert_eq!(d.names, vec!["foo", "x", "y"]);
        assert!(d.body.is_none());
        assert!(d.ty.is_some());
    }

    #[test]
    fn test_semicolon_without_spaces() {
        let result = cell("a;b");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        assert!(matches!(c, Cell::Comp(1, _)));
    }

    #[test]
    fn test_semicolon_with_spaces() {
        let result = cell("a ; b");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        assert!(matches!(c, Cell::Comp(1, _)));
    }

    #[test]
    fn test_ident_with_dot() {
        let result = ident("foo.bar");
        assert!(result.is_ok());
        let (_, name) = result.unwrap();
        assert_eq!(name, "foo.bar");
    }

    #[test]
    fn test_precedence_simple() {
        // a ; b ;; c should parse as (a ; b) ;; c
        let result = cell("a ; b ;; c");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        assert_eq!(
            c,
            Cell::Comp(
                2,
                vec![
                    Cell::Comp(
                        1,
                        vec![
                            Cell::Var("a".to_string(), vec![]),
                            Cell::Var("b".to_string(), vec![]),
                        ]
                    ),
                    Cell::Var("c".to_string(), vec![]),
                ]
            )
        );
    }

    #[test]
    fn test_precedence_reversed() {
        // a ;; b ; c should parse as a ;; (b ; c)
        let result = cell("a ;; b ; c");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        assert_eq!(
            c,
            Cell::Comp(
                2,
                vec![
                    Cell::Var("a".to_string(), vec![]),
                    Cell::Comp(
                        1,
                        vec![
                            Cell::Var("b".to_string(), vec![]),
                            Cell::Var("c".to_string(), vec![]),
                        ]
                    ),
                ]
            )
        );
    }

    #[test]
    fn test_zero_semicolon_composition() {
        // a b (space separated, no semicolon) should parse as Comp(0, ...)
        let result = cell("a b");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        assert_eq!(
            c,
            Cell::Comp(
                0,
                vec![
                    Cell::Var("a".to_string(), vec![]),
                    Cell::Var("b".to_string(), vec![]),
                ]
            )
        );
    }

    #[test]
    fn test_zero_semicolon_precedence() {
        // a b ; c should parse as (a b) ; c
        let result = cell("a b ; c");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        assert_eq!(
            c,
            Cell::Comp(
                1,
                vec![
                    Cell::Comp(
                        0,
                        vec![
                            Cell::Var("a".to_string(), vec![]),
                            Cell::Var("b".to_string(), vec![]),
                        ]
                    ),
                    Cell::Var("c".to_string(), vec![]),
                ]
            )
        );
    }

    #[test]
    fn test_zero_semicolon_three_items() {
        // a b c should parse as Comp(0, [a, Comp(0, [b, c])])
        // Due to right-associativity (higher precedence on right)
        let result = cell("a b c");
        assert!(result.is_ok());
        let (_, c) = result.unwrap();
        assert_eq!(
            c,
            Cell::Comp(
                0,
                vec![
                    Cell::Var("a".to_string(), vec![]),
                    Cell::Comp(
                        0,
                        vec![
                            Cell::Var("b".to_string(), vec![]),
                            Cell::Var("c".to_string(), vec![]),
                        ]
                    ),
                ]
            )
        );
    }

    #[test]
    fn test_ident_with_number() {
        let result = ident("map-2d");
        eprintln!("Result: {:?}", result);
        assert!(result.is_ok());
        let (_, name) = result.unwrap();
        assert_eq!(name, "map-2d");
    }

    #[test]
    fn test_parse_map_2d_decl() {
        let result = decl("map-2d f arr = row-map[f]");
        eprintln!("Result: {:?}", result);
        assert!(result.is_ok());
        let (_, d) = result.unwrap();
        assert_eq!(d.names[0], "map-2d");
    }

    #[test]
    fn test_simple_three_decls() {
        let input = "a = b\nc = d\ne = f";
        let result = program(input);
        eprintln!("Result: {:?}", result);
        assert!(result.is_ok());
        let (_, decls) = result.unwrap();
        eprintln!("Decls: {}", decls.len());
        assert_eq!(decls.len(), 3);
    }

    #[test]
    fn test_map_2d_standalone() {
        let input = "map-2d f arr = row-map[f]";
        let result = decl(input);
        eprintln!("Result: {:?}", result);
        assert!(result.is_ok());
        let (_, d) = result.unwrap();
        assert_eq!(d.names, vec!["map-2d", "f", "arr"]);
    }

    #[test]
    fn test_two_decls_simple() {
        let input = "compose [f g] = f[g]\n\nmap-2d f arr = row-map[f]";
        let result = program(input);
        eprintln!("Result OK: {}", result.is_ok());
        if let Ok((rest, decls)) = &result {
            eprintln!("Rest: {:?}", rest);
            eprintln!("Decls: {}", decls.len());
            for (i, d) in decls.iter().enumerate() {
                eprintln!("  decl[{}]: {:?}", i, d.names);
            }
        }
        assert!(result.is_ok());
        let (_, decls) = result.unwrap();
        assert_eq!(decls.len(), 2);
    }

    #[test]
    fn test_actual_complex_input() {
        let input = r#"
            [inline]
            [pure]
            identity [x: *] : * = x

            compose [f g] = f[g]

            map-2d f arr = row-map[f] ; col-map[f] ; arr
        "#;
        eprintln!("Input: {:?}", input);
        eprintln!("Input bytes: {:?}", input.as_bytes());
        let result = program(input);
        eprintln!("Result OK: {}", result.is_ok());
        if let Ok((rest, decls)) = &result {
            eprintln!("Rest: {:?}", rest);
            eprintln!("Decls: {}", decls.len());
            for (i, d) in decls.iter().enumerate() {
                eprintln!("  decl[{}]: {:?}", i, d.names);
            }
        }
    }

    #[test]
    fn test_assoc() {
        let input = r#"
            u: *
            x: u → u
            m: x x → x
            assoc: x m; m → m x; m
            rm = x m; m
            lm = m x; m
            assoc2: rm → lm
        "#;
        let result = parse_program(input);
        eprintln!("Result: {:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_with_composition() {
        // Test that cell_type can handle compositions on both sides of →
        let input = "f: x y → a b";
        let result = parse_program(input);
        eprintln!("Result: {:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_with_semicolon() {
        // Test that cell_type can handle semicolon operators
        let input = "f: x; y → a; b";
        let result = parse_program(input);
        eprintln!("Result: {:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_with_parens() {
        // Test the actual problematic line from pentagon
        let input = "chl: (x m; m) x → x m x; m x";
        let result = parse_program(input);
        eprintln!("Result: {:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn test_pentagon_first_few() {
        let input = r#"
            u: *
            x: u → u
            m: x x → x
            a: m x; m → x m; m
            chl: (x m; m) x → x m x; m x
            chr: x m x; x m → x (m x; m)
        "#;
        let result = parse_program(input);
        eprintln!("Result: {:?}", result);
        if let Err(e) = &result {
            eprintln!("Error: {}", e);
        }
        assert!(result.is_ok());
    }

    #[test]
    fn test_pentagon() {
        let input = r#"
            u: *
            x: u → u
            m: x x → x
            a: m x; m → x m; m
            chl: (x m; m) x → x m x; m x
            chr: x m x; x m → x (m x; m)
            aaa =
                a x; m ;;
                chl; m ;;
                x m x; a ;;
                chr; m ;;
                x a; m
            ch0: m x x; x m → m m
            ch1: m m → x x m; m x

            kl: (m x; m) x → m x x; m x
            kr: x x m; x m → x (x m; m)
            oao =
                kl; m ;;
                m x x; a ;;
                (ch0 ;; ch1); m ;;
                x x m; a ;;
                kr; m

            pentagon: aaa → oao
        "#;
        let result = parse_program(input);
        eprintln!("Result: {:?}", result);
        assert!(result.is_ok());
    }
}
