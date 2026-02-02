// Parser implementation created by Claude Code
// https://claude.ai/code

use crate::types::*;
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, digit1, multispace0, multispace1, space0},
    combinator::{map, opt, recognize},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated},
};

/*
Grammar:

decl = name (name* | '[' arg-decls ']') (':' cell-type)? ('=' cell)?
     | '[' decorator % ',' ']' decl
arg-decls = (name+ (':' arg-type)?) % ','
cell-type = '*'
          | cell '→' cell
lit-type = 'nat' | 'int' | 'rat' | 'str'
lit = nat | int | rat | str
arg-type = cell-type | lit-type
arg = cell | lit
cell = name ('[' (arg % ',') ']')?
     | cell (';'* | ';' nat ';') cell
     | '(' cell ')'
decorator = name ('(' (arg % ',') ')')?
program = decl*

Whitespace handling strategy:
- Declarations are delimited by newlines
- Within cells, space-separated tokens form compositions (axis 0)
- Newlines stop cell composition (to separate declarations)
- After semicolon operators (;, ;;, etc.), newlines are allowed (for multi-line bodies)
- After '=' in declarations, newlines are allowed (for multi-line bodies)
- After ']' in cell arguments, newlines are NOT consumed (to preserve declaration boundaries)
*/

// ============================================================================
// Helper functions
// ============================================================================

// Check if a character is valid inside an identifier (not at the start)
fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-' || c == '.' || c == '\''
}

// Check if a character can start an identifier
fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

// Check if a string is a valid identifier
fn is_valid_ident(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let first = s.chars().next().unwrap();
    is_ident_start(first) && s.chars().all(is_ident_char)
}

// Validate identifier and return error if invalid
fn validate_ident<'a>(name: &str, input: &'a str) -> IResult<&'a str, ()> {
    if is_valid_ident(name) {
        Ok((input, ()))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alpha,
        )))
    }
}

// Helper: skip all whitespace (including newlines)
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

// Check if input starts with a newline
fn starts_with_newline(s: &str) -> bool {
    s.starts_with('\n') || s.starts_with("\r\n")
}

// ============================================================================
// Identifier parsers
// ============================================================================

// Parse identifier with all whitespace (including newlines)
fn ident(input: &str) -> IResult<&str, Ident> {
    let (input, _) = multispace0(input)?;
    let (input, name) = recognize(take_while1(is_ident_char)).parse(input)?;
    validate_ident(name, input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, name.to_string()))
}

// Parse identifier with horizontal whitespace only (not newlines)
// Used in cells and declarations where newlines are significant
fn ident_hspace(input: &str) -> IResult<&str, Ident> {
    let (input, _) = space0(input)?;
    let (input, name) = recognize(take_while1(is_ident_char)).parse(input)?;
    validate_ident(name, input)?;
    let (input, _) = space0(input)?;
    Ok((input, name.to_string()))
}

// Parse identifier without any whitespace handling
// Used in separated_list where the separator handles whitespace
fn ident_raw(input: &str) -> IResult<&str, Ident> {
    let (input, name) = recognize(take_while1(is_ident_char)).parse(input)?;
    validate_ident(name, input)?;
    Ok((input, name.to_string()))
}

// ============================================================================
// Literal parsers
// ============================================================================

// nat literal
fn nat_lit(input: &str) -> IResult<&str, Lit> {
    ws(map(digit1, |n: &str| Lit::Nat(n.parse().unwrap()))).parse(input)
}

// int literal (signed) - only matches if there's actually a minus sign
fn int_lit(input: &str) -> IResult<&str, Lit> {
    ws(map(pair(char('-'), digit1), |(_, n): (char, &str)| {
        Lit::Int(format!("-{}", n).parse().unwrap())
    }))
    .parse(input)
}

// rat literal (float)
fn rat_lit(input: &str) -> IResult<&str, Lit> {
    ws(map(
        (opt(char('-')), digit1, char('.'), digit1),
        |(sign, int_part, _, frac_part): (Option<char>, &str, char, &str)| {
            let s = if sign.is_some() {
                format!("-{}.{}", int_part, frac_part)
            } else {
                format!("{}.{}", int_part, frac_part)
            };
            Lit::Rat(s.parse().unwrap())
        },
    ))
    .parse(input)
}

// str literal
fn str_lit(input: &str) -> IResult<&str, Lit> {
    ws(map(
        delimited(char('"'), take_while(|c| c != '"'), char('"')),
        |s: &str| Lit::Str(s.to_string()),
    ))
    .parse(input)
}

// lit
fn lit(input: &str) -> IResult<&str, Lit> {
    alt((rat_lit, int_lit, nat_lit, str_lit)).parse(input)
}

// lit-type
fn lit_type(input: &str) -> IResult<&str, LitType> {
    ws(map(
        alt((tag("nat"), tag("int"), tag("rat"), tag("str"))),
        |name: &str| match name {
            "nat" => LitType::Nat,
            "int" => LitType::Int,
            "rat" => LitType::Rat,
            "str" => LitType::Str,
            _ => unreachable!(),
        },
    ))
    .parse(input)
}

// ============================================================================
// Cell parsers
// ============================================================================

fn cell_primary(input: &str) -> IResult<&str, Cell> {
    alt((
        // '(' cell ')'
        // Don't skip whitespace after ')' to preserve declaration boundaries
        map(
            delimited(
                preceded(multispace0, char('(')),
                preceded(multispace0, cell),
                preceded(multispace0, char(')')),
            ),
            |c| c,
        ),
        // name ('[' (arg % ',') ']')?
        map(
            pair(
                ident_hspace,
                opt(delimited(
                    ws(char('[')),
                    separated_list0(ws(char(',')), arg),
                    // Don't skip whitespace after ']' to preserve declaration boundaries
                    preceded(multispace0, char(']')),
                )),
            ),
            |(name, args)| Cell::Var(name, args.unwrap_or_default()),
        ),
    ))
    .parse(input)
}

fn cell(input: &str) -> IResult<&str, Cell> {
    cell_with_precedence(input, 255)
}

// Parse semicolon operator: ';', ';;', ';;;', ... or ';N;'
// Returns the axis number (count of semicolons, or N for ;N; format)
fn semicolon_operator(input: &str) -> IResult<&str, u8> {
    let (input, _) = space0(input)?; // Skip horizontal whitespace before

    let (input, axis) = alt((
        // ;N; format (explicit axis number)
        map(
            delimited(char(';'), digit1, char(';')),
            |n: &str| n.parse::<u8>().unwrap(),
        ),
        // ;;; format (count semicolons)
        map(many1(char(';')), |semis| semis.len() as u8),
    ))
    .parse(input)?;

    // Skip all whitespace after (including newlines)
    // This allows multi-line cell bodies
    let (input, _) = multispace0(input)?;
    Ok((input, axis))
}

fn cell_with_precedence(input: &str, min_precedence: u8) -> IResult<&str, Cell> {
    let (input, left) = cell_primary(input)?;
    cell_with_precedence_inner(input, left, min_precedence)
}

// Precedence climbing algorithm for cell composition
// Lower axis number = higher precedence (binds tighter)
// Right-associative: a ; b ; c parses as a ; (b ; c)
fn cell_with_precedence_inner(
    mut input: &str,
    mut left: Cell,
    min_precedence: u8,
) -> IResult<&str, Cell> {
    loop {
        let before_op = input;

        // Try to find an operator (semicolon or space-separated)
        let axis = match semicolon_operator(input) {
            Ok((rest, axis)) => {
                input = rest;
                Some(axis)
            }
            Err(_) => {
                // No explicit semicolon operator
                // Check for space-separated composition (axis 0)
                // Stop if we encounter a newline (declarations are separated by newlines)
                if starts_with_newline(input) {
                    None
                } else {
                    match cell_primary(input) {
                        Ok(_) => Some(0), // Space-separated composition
                        Err(_) => None,
                    }
                }
            }
        };

        let axis = match axis {
            Some(a) => a,
            None => {
                input = before_op;
                break;
            }
        };

        // Check precedence: lower axis = higher precedence
        if axis >= min_precedence {
            input = before_op;
            break;
        }

        // Parse right side with right-associativity (axis + 1)
        let (rest, right) = cell_with_precedence(input, axis + 1)?;
        input = rest;
        left = Cell::Comp(axis, vec![left, right]);
    }

    Ok((input, left))
}

// ============================================================================
// Type and argument parsers
// ============================================================================

fn arg(input: &str) -> IResult<&str, Arg> {
    alt((map(lit, Arg::Lit), map(cell, Arg::Cell))).parse(input)
}

fn arg_type(input: &str) -> IResult<&str, ArgType> {
    alt((map(lit_type, ArgType::Lit), map(cell_type, ArgType::Cell))).parse(input)
}

fn cell_type(input: &str) -> IResult<&str, CellType> {
    alt((
        map(ws(char('*')), |_| CellType::Star),
        map((cell, ws(tag("→")), cell), |(left, _, right)| {
            CellType::Arr(left, right)
        }),
    ))
    .parse(input)
}

fn arg_decl(input: &str) -> IResult<&str, ArgDecl> {
    let (input, names) = separated_list1(multispace1, ident_raw).parse(input)?;
    let (input, _) = multispace0(input)?;
    let (input, ty) = opt(preceded(ws(char(':')), arg_type)).parse(input)?;
    Ok((input, ArgDecl { names, ty }))
}

fn arg_decls(input: &str) -> IResult<&str, Vec<ArgDecl>> {
    separated_list0(ws(char(',')), arg_decl).parse(input)
}

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

// ============================================================================
// Declaration parsers
// ============================================================================

// Parse declaration names until '[', ':', '=', or newline
// Returns a list of names (for multi-name declarations like "foo x y = ...")
fn decl_names(input: &str) -> IResult<&str, Vec<Ident>> {
    let (mut input, first) = ident_hspace(input)?;
    let mut names = vec![first];

    loop {
        // Stop at newline or special characters
        if starts_with_newline(input) {
            break;
        }

        let (rest, _) = space0(input)?;
        if rest.starts_with('[') || rest.starts_with(':') || rest.starts_with('=') {
            break;
        }
        if starts_with_newline(rest) {
            break;
        }

        // Try to parse another identifier
        match ident_raw(rest) {
            Ok((rest2, name)) => {
                input = rest2;
                names.push(name);
            }
            Err(_) => break,
        }
    }

    Ok((input, names))
}

// Parse a declaration without decorators
// Handles both forms:
//   foo x y = ...         (multiple names, no brackets)
//   foo [x y: nat] = ...  (single name with bracketed args)
fn simple_decl(input: &str) -> IResult<&str, Decl> {
    let (mut input, names) = decl_names(input)?;

    // Skip horizontal whitespace
    input = space0(input)?.0;

    // Parse bracketed argument declarations (only valid for single-name decls)
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

    input = space0(input)?.0;

    // Parse type annotation
    let (mut input, ty) = if input.starts_with(':') {
        let (input, ty) = preceded(char(':'), preceded(space0, cell_type)).parse(input)?;
        (input, Some(ty))
    } else {
        (input, None)
    };

    input = space0(input)?.0;

    // Parse body (allow newlines after '=' for multi-line bodies)
    let (input, body) = if input.starts_with('=') {
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

// Parse a declaration (with optional decorators)
fn decl(input: &str) -> IResult<&str, Decl> {
    let with_decorators = map(
        (
            delimited(
                ws(char('[')),
                separated_list1(ws(char(',')), decorator),
                ws(char(']')),
            ),
            decl, // Recursive: decorators can nest
        ),
        |(mut decos, mut d)| {
            // Prepend new decorators to existing ones
            decos.append(&mut d.decos);
            d.decos = decos;
            d
        },
    );

    alt((with_decorators, simple_decl)).parse(input)
}

// ============================================================================
// Program parser
// ============================================================================

fn program(input: &str) -> IResult<&str, Vec<Decl>> {
    let (input, _) = multispace0(input)?;
    let (input, decls) = many0(terminated(decl, multispace0)).parse(input)?;
    Ok((input, decls))
}

/// Public interface for parsing a complete program
/// Returns an error if the input is not fully consumed
pub fn parse_program(input: &str) -> std::result::Result<Vec<Decl>, String> {
    match program(input) {
        Ok((remaining, decls)) => {
            if !remaining.is_empty() {
                Err(format!(
                    "Parse error: unexpected input remaining: {:?}",
                    &remaining[..remaining.len().min(50)]
                ))
            } else {
                Ok(decls)
            }
        }
        Err(e) => Err(format!("Parse error: {}", e)),
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_ident() {
        let result = ident("foo");
        let (rest, name) = result.unwrap();
        assert_eq!(rest, "");
        assert_eq!(name, "foo");
    }

    #[test]
    fn test_ident_with_dash() {
        let result = ident("foo-bar");
        let (_, name) = result.unwrap();
        assert_eq!(name, "foo-bar");
    }

    #[test]
    fn test_nat_literal() {
        let result = nat_lit("123");
        let (_, lit) = result.unwrap();
        assert!(matches!(lit, Lit::Nat(123)));
    }

    #[test]
    fn test_int_literal() {
        let result = int_lit("-42");
        let (_, lit) = result.unwrap();
        assert!(matches!(lit, Lit::Int(-42)));
    }

    #[test]
    fn test_rat_literal() {
        let result = rat_lit("3.14");
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
        let (_, lit) = result.unwrap();
        assert!(matches!(lit, Lit::Str(s) if s == "hello world"));
    }

    #[test]
    fn test_simple_cell() {
        let result = cell("foo");
        let (_, c) = result.unwrap();
        assert_eq!(c, Cell::Var("foo".to_string(), vec![]));
    }

    #[test]
    fn test_cell_with_args() {
        let result = cell("foo[1, 2]");
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
        let (_, c) = result.unwrap();
        let expected = Cell::Comp(
            2,
            vec![Cell::Var("a".to_string(), vec![]), Cell::Var("b".to_string(), vec![])],
        );
        assert_eq!(c, expected);
    }

    #[test]
    fn test_cell_composition_with_axis() {
        let result = cell("a ;3; b");
        let (_, c) = result.unwrap();
        let expected = Cell::Comp(
            3,
            vec![Cell::Var("a".to_string(), vec![]), Cell::Var("b".to_string(), vec![])],
        );
        assert_eq!(c, expected);
    }

    #[test]
    fn test_cell_composition_complex() {
        let result = cell("a ; b ;; c");
        let (_, c) = result.unwrap();
        // Should be: (a ; b) ;; c
        // Because ; has higher precedence than ;;
        let expected = Cell::Comp(
            2,
            vec![
                Cell::Comp(
                    1,
                    vec![Cell::Var("a".to_string(), vec![]), Cell::Var("b".to_string(), vec![])],
                ),
                Cell::Var("c".to_string(), vec![]),
            ],
        );
        assert_eq!(c, expected);
    }

    #[test]
    fn test_cell_with_parens() {
        let result = cell("(foo)");
        let (_, c) = result.unwrap();
        let expected = Cell::Var("foo".to_string(), vec![]);
        assert_eq!(c, expected);
    }

    #[test]
    fn test_simple_decl() {
        let result = decl("foo x y = bar");
        let (_, d) = result.unwrap();
        let expected = Decl {
            decos: vec![],
            names: vec!["foo".to_string(), "x".to_string(), "y".to_string()],
            args: vec![],
            ty: None,
            body: Some(Cell::Var("bar".to_string(), vec![])),
        };
        assert_eq!(d, expected);
    }

    #[test]
    fn test_arg_decls_simple() {
        let result = arg_decls("x y: nat, z: int");
        let (_, args) = result.unwrap();
        let expected = vec![
            ArgDecl {
                names: vec!["x".to_string(), "y".to_string()],
                ty: Some(ArgType::Lit(LitType::Nat)),
            },
            ArgDecl {
                names: vec!["z".to_string()],
                ty: Some(ArgType::Lit(LitType::Int)),
            },
        ];
        assert_eq!(args, expected);
    }

    #[test]
    fn test_decl_with_brackets() {
        let result = decl("foo [x y: nat, z: int] = bar");
        let (_, d) = result.unwrap();
        let expected = Decl {
            decos: vec![],
            names: vec!["foo".to_string()],
            args: vec![
                ArgDecl {
                    names: vec!["x".to_string(), "y".to_string()],
                    ty: Some(ArgType::Lit(LitType::Nat)),
                },
                ArgDecl {
                    names: vec!["z".to_string()],
                    ty: Some(ArgType::Lit(LitType::Int)),
                },
            ],
            ty: None,
            body: Some(Cell::Var("bar".to_string(), vec![])),
        };
        assert_eq!(d, expected);
    }

    #[test]
    fn test_decl_with_type() {
        let result = decl("foo : * = bar");
        let (_, d) = result.unwrap();
        let expected = Decl {
            decos: vec![],
            names: vec!["foo".to_string()],
            args: vec![],
            ty: Some(CellType::Star),
            body: Some(Cell::Var("bar".to_string(), vec![])),
        };
        assert_eq!(d, expected);
    }

    #[test]
    fn test_decl_with_decorator() {
        let result = decl("[inline] foo = bar");
        let (_, d) = result.unwrap();
        let expected = Decl {
            decos: vec![Decorator {
                name: "inline".to_string(),
                args: vec![],
            }],
            names: vec!["foo".to_string()],
            args: vec![],
            ty: None,
            body: Some(Cell::Var("bar".to_string(), vec![])),
        };
        assert_eq!(d, expected);
    }

    #[test]
    fn test_decl_with_multiple_decorators() {
        let result = decl("[inline, pure] foo = bar");
        let (_, d) = result.unwrap();
        let expected = Decl {
            decos: vec![
                Decorator {
                    name: "inline".to_string(),
                    args: vec![],
                },
                Decorator {
                    name: "pure".to_string(),
                    args: vec![],
                },
            ],
            names: vec!["foo".to_string()],
            args: vec![],
            ty: None,
            body: Some(Cell::Var("bar".to_string(), vec![])),
        };
        assert_eq!(d, expected);
    }

    #[test]
    fn test_decl_with_multiple_decorator_brackets() {
        let result = decl("[inline] [pure] foo = bar");
        let (_, d) = result.unwrap();
        let expected = Decl {
            decos: vec![
                Decorator {
                    name: "inline".to_string(),
                    args: vec![],
                },
                Decorator {
                    name: "pure".to_string(),
                    args: vec![],
                },
            ],
            names: vec!["foo".to_string()],
            args: vec![],
            ty: None,
            body: Some(Cell::Var("bar".to_string(), vec![])),
        };
        assert_eq!(d, expected);
    }

    #[test]
    fn test_decorator_with_args() {
        let result = decl("[optimize(3)] foo = bar");
        let (_, d) = result.unwrap();
        let expected = Decl {
            decos: vec![Decorator {
                name: "optimize".to_string(),
                args: vec![Arg::Lit(Lit::Nat(3))],
            }],
            names: vec!["foo".to_string()],
            args: vec![],
            ty: None,
            body: Some(Cell::Var("bar".to_string(), vec![])),
        };
        assert_eq!(d, expected);
    }

    #[test]
    fn test_program_single_decl() {
        let result = program("foo = bar");
        let (_, decls) = result.unwrap();
        let expected = vec![Decl {
            decos: vec![],
            names: vec!["foo".to_string()],
            args: vec![],
            ty: None,
            body: Some(Cell::Var("bar".to_string(), vec![])),
        }];
        assert_eq!(decls, expected);
    }

    #[test]
    fn test_program_multiple_decls() {
        let result = program("foo = bar\nbaz = qux");
        let (_, decls) = result.unwrap();
        let expected = vec![
            Decl {
                decos: vec![],
                names: vec!["foo".to_string()],
                args: vec![],
                ty: None,
                body: Some(Cell::Var("bar".to_string(), vec![])),
            },
            Decl {
                decos: vec![],
                names: vec!["baz".to_string()],
                args: vec![],
                ty: None,
                body: Some(Cell::Var("qux".to_string(), vec![])),
            },
        ];
        assert_eq!(decls, expected);
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
        let (_, decls) = result.unwrap();

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
        let (_, c) = result.unwrap();
        let expected = Cell::Comp(
            2,
            vec![
                Cell::Comp(
                    1,
                    vec![Cell::Var("a".to_string(), vec![]), Cell::Var("b".to_string(), vec![])],
                ),
                Cell::Comp(
                    1,
                    vec![Cell::Var("c".to_string(), vec![]), Cell::Var("d".to_string(), vec![])],
                ),
            ],
        );
        assert_eq!(c, expected);
    }

    #[test]
    fn test_cell_with_complex_args() {
        let result = cell("foo[a ; b, \"hello\", 42]");
        let (_, c) = result.unwrap();
        let expected = Cell::Var(
            "foo".to_string(),
            vec![
                Arg::Cell(Cell::Comp(
                    1,
                    vec![Cell::Var("a".to_string(), vec![]), Cell::Var("b".to_string(), vec![])],
                )),
                Arg::Lit(Lit::Str("hello".to_string())),
                Arg::Lit(Lit::Nat(42)),
            ],
        );
        assert_eq!(c, expected);
    }

    #[test]
    fn test_arrow_type() {
        let result = cell_type("a → b");
        let (_, ty) = result.unwrap();
        let expected = CellType::Arr(
            Cell::Var("a".to_string(), vec![]),
            Cell::Var("b".to_string(), vec![]),
        );
        assert_eq!(ty, expected);
    }

    #[test]
    fn test_empty_program() {
        let result = program("");
        let (_, decls) = result.unwrap();
        let expected: Vec<Decl> = vec![];
        assert_eq!(decls, expected);
    }

    #[test]
    fn test_decl_no_body() {
        let result = decl("foo x y : *");
        let (_, d) = result.unwrap();
        let expected = Decl {
            decos: vec![],
            names: vec!["foo".to_string(), "x".to_string(), "y".to_string()],
            args: vec![],
            ty: Some(CellType::Star),
            body: None,
        };
        assert_eq!(d, expected);
    }

    #[test]
    fn test_semicolon_without_spaces() {
        let result = cell("a;b");
        let (_, c) = result.unwrap();
        let expected = Cell::Comp(
            1,
            vec![Cell::Var("a".to_string(), vec![]), Cell::Var("b".to_string(), vec![])],
        );
        assert_eq!(c, expected);
    }

    #[test]
    fn test_semicolon_with_spaces() {
        let result = cell("a ; b");
        let (_, c) = result.unwrap();
        let expected = Cell::Comp(
            1,
            vec![Cell::Var("a".to_string(), vec![]), Cell::Var("b".to_string(), vec![])],
        );
        assert_eq!(c, expected);
    }

    #[test]
    fn test_ident_with_dot() {
        let result = ident("foo.bar");
        let (_, name) = result.unwrap();
        assert_eq!(name, "foo.bar");
    }

    #[test]
    fn test_ident_with_apostrophe() {
        let result = ident("foo'");
        let (_, name) = result.unwrap();
        assert_eq!(name, "foo'");
    }

    #[test]
    fn test_ident_with_multiple_apostrophes() {
        let result = ident("f''");
        let (_, name) = result.unwrap();
        assert_eq!(name, "f''");
    }

    #[test]
    fn test_precedence_simple() {
        // a ; b ;; c should parse as (a ; b) ;; c
        let result = cell("a ; b ;; c");
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
        assert!(result.is_ok());
        let (_, d) = result.unwrap();
        let expected = Decl {
            decos: vec![],
            names: vec!["map-2d".to_string(), "f".to_string(), "arr".to_string()],
            args: vec![],
            ty: None,
            body: Some(Cell::Var("row-map".to_string(), vec![Arg::Cell(Cell::Var("f".to_string(), vec![]))])),
        };
        assert_eq!(d, expected);
    }

    #[test]
    fn test_simple_three_decls() {
        let input = "a = b\nc = d\ne = f";
        let result = program(input);
        assert!(result.is_ok());
        let (_, decls) = result.unwrap();
        let expected = vec![
            Decl {
                decos: vec![],
                names: vec!["a".to_string()],
                args: vec![],
                ty: None,
                body: Some(Cell::Var("b".to_string(), vec![])),
            },
            Decl {
                decos: vec![],
                names: vec!["c".to_string()],
                args: vec![],
                ty: None,
                body: Some(Cell::Var("d".to_string(), vec![])),
            },
            Decl {
                decos: vec![],
                names: vec!["e".to_string()],
                args: vec![],
                ty: None,
                body: Some(Cell::Var("f".to_string(), vec![])),
            },
        ];
        assert_eq!(decls, expected);
    }

    #[test]
    fn test_map_2d_standalone() {
        let input = "map-2d f arr = row-map[f]";
        let result = decl(input);
        assert!(result.is_ok());
        let (_, d) = result.unwrap();
        let expected = Decl {
            decos: vec![],
            names: vec!["map-2d".to_string(), "f".to_string(), "arr".to_string()],
            args: vec![],
            ty: None,
            body: Some(Cell::Var("row-map".to_string(), vec![Arg::Cell(Cell::Var("f".to_string(), vec![]))])),
        };
        assert_eq!(d, expected);
    }

    #[test]
    fn test_two_decls_simple() {
        let input = "compose [f g] = f[g]\n\nmap-2d f arr = row-map[f]";
        let result = program(input);
        assert!(result.is_ok());
        let (_, decls) = result.unwrap();
        let expected = vec![
            Decl {
                decos: vec![],
                names: vec!["compose".to_string()],
                args: vec![ArgDecl {
                    names: vec!["f".to_string(), "g".to_string()],
                    ty: None,
                }],
                ty: None,
                body: Some(Cell::Var("f".to_string(), vec![Arg::Cell(Cell::Var("g".to_string(), vec![]))])),
            },
            Decl {
                decos: vec![],
                names: vec!["map-2d".to_string(), "f".to_string(), "arr".to_string()],
                args: vec![],
                ty: None,
                body: Some(Cell::Var("row-map".to_string(), vec![Arg::Cell(Cell::Var("f".to_string(), vec![]))])),
            },
        ];
        assert_eq!(decls, expected);
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
    fn test_simple_arrow() {
        // Simple arrow type (multi-arrows are complex to parse with current grammar)
        let input = "f: a → b";
        let result = parse_program(input);
        assert!(result.is_ok());
        let decls = result.unwrap();
        let expected = vec![Decl {
            decos: vec![],
            names: vec!["f".to_string()],
            args: vec![],
            ty: Some(CellType::Arr(
                Cell::Var("a".to_string(), vec![]),
                Cell::Var("b".to_string(), vec![]),
            )),
            body: None,
        }];
        assert_eq!(decls, expected);
    }

    #[test]
    fn test_long_composition() {
        // Many terms in space-separated composition - right associative
        // a b c d = a (b (c d))
        let input = "f = a b c d";
        let result = parse_program(input);
        assert!(result.is_ok());
        let decls = result.unwrap();
        let expected_body = Cell::Comp(
            0,
            vec![
                Cell::Var("a".to_string(), vec![]),
                Cell::Comp(
                    0,
                    vec![
                        Cell::Var("b".to_string(), vec![]),
                        Cell::Comp(
                            0,
                            vec![
                                Cell::Var("c".to_string(), vec![]),
                                Cell::Var("d".to_string(), vec![]),
                            ],
                        ),
                    ],
                ),
            ],
        );
        assert_eq!(decls[0].body, Some(expected_body));
    }

    #[test]
    fn test_mixed_axes() {
        // Mix different axis numbers
        // Lower axis binds tighter: ((a ; b) ;; c) ;;; d) ;4; e
        let input = "f = a ; b ;; c ;;; d ;4; e";
        let result = parse_program(input);
        assert!(result.is_ok());
        let decls = result.unwrap();
        let expected_body = Cell::Comp(
            4,
            vec![
                Cell::Comp(
                    3,
                    vec![
                        Cell::Comp(
                            2,
                            vec![
                                Cell::Comp(
                                    1,
                                    vec![
                                        Cell::Var("a".to_string(), vec![]),
                                        Cell::Var("b".to_string(), vec![]),
                                    ],
                                ),
                                Cell::Var("c".to_string(), vec![]),
                            ],
                        ),
                        Cell::Var("d".to_string(), vec![]),
                    ],
                ),
                Cell::Var("e".to_string(), vec![]),
            ],
        );
        assert_eq!(decls[0].body, Some(expected_body));
    }

    #[test]
    fn test_deeply_nested_parens() {
        // Deeply nested parentheses
        let input = "f = ((((a b) c) d) e)";
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_complex_args_in_cell() {
        // Complex arguments with compositions
        let input = "f = g[a ; b, c d, (e f)]";
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_multiline_with_deep_indent() {
        // Multi-line body with various indentation
        let input = r#"
            f =
                a ;;
                    b ;;
                        c ;;
                    d ;;
                e
        "#;
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_decorator_with_complex_args() {
        // Decorators with complex cell arguments
        let input = "[transform(f ; g, h[x y])] foo = bar";
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_many_decorators() {
        // Many decorator brackets
        let input = "[a][b][c][d][e][f] foo = bar";
        let result = parse_program(input);
        assert!(result.is_ok());
        if let Ok(decls) = result {
            assert_eq!(decls[0].decos.len(), 6);
        }
    }

    #[test]
    fn test_type_only_decl() {
        // Declaration with only type, no body
        let input = "f: *";
        let result = parse_program(input);
        assert!(result.is_ok());
        let decls = result.unwrap();
        let expected = vec![Decl {
            decos: vec![],
            names: vec!["f".to_string()],
            args: vec![],
            ty: Some(CellType::Star),
            body: None,
        }];
        assert_eq!(decls, expected);
    }

    #[test]
    fn test_multiple_names_with_type() {
        // Multiple names with type annotation
        let input = "f g h: *";
        let result = parse_program(input);
        assert!(result.is_ok());
        if let Ok(decls) = result {
            assert_eq!(decls[0].names.len(), 3);
        }
    }

    #[test]
    fn test_zero_axis_with_other_axes() {
        // Space-separated (axis 0) mixed with explicit semicolons
        let input = "f = a b ; c d ; e f";
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_very_long_identifier() {
        // Long identifier with various allowed characters
        let input = "very-long_identifier.with.dots-and_underscores = x";
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_mathematical_notation_with_primes() {
        // Common mathematical notation with primes (f', f'', etc.)
        let input = r#"
            f: *
            f' [x: *] : * = f[x]
            f'' [x: *] : * = f'[x]
            compose_with_derivative = f ; f' ; f''
        "#;
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_string_literals_in_args() {
        // String literals as arguments
        let input = r#"f = g["hello", "world", "test"]"#;
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_numeric_literals_in_composition() {
        // Using numeric literals in compositions
        let input = "f = g[1, -2, 3.14, -9.8]";
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_composition_in_type() {
        // Composition on both sides of arrow in type
        let input = "f: a b c → d e f";
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_empty_brackets() {
        // Empty argument lists
        let input = "f[] = x";
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_many_arg_decls() {
        // Many argument declarations
        let input = "f [a b: nat, c d: int, e: rat, f g h: str] = x";
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_simple_type_decls() {
        // Simple type declarations
        let input = r#"
            nat: *
            compose_nat [α β: *] : *
        "#;
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_functor_composition() {
        // Functor composition with mixed operators
        let input = r#"
            comp = f ; g
            comp2 = f g
            comp3 = f ;; g
            mixed = (f g) ; (h i) ;; j
        "#;
        let result = parse_program(input);
        assert!(result.is_ok());
        if let Ok(decls) = result {
            assert_eq!(decls.len(), 4);
        }
    }

    #[test]
    fn test_monoidal_unit() {
        // Monoidal category with unit and tensor (simplified without parens in types)
        let input = r#"
            unit: *
            tensor [a b: *] : *
            assoc [a b c: *] : *
            left_unit [a: *] : *
            right_unit [a: *] : *
        "#;
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_unicode_arrow() {
        // Make sure → (Unicode arrow) works properly with spaces
        let input = "f: a → b";
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_explicit_axis_zero() {
        // Explicit ;0; should be same as space
        let input = "f = a ;0; b ;0; c";
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_high_axis_numbers() {
        // High axis numbers (single digits work, multi-digit may need more complex parsing)
        let input = "f = a ;9; b";
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_whitespace_variations() {
        // Various whitespace patterns
        let input = "f = a ; b\ng   =   c   ;;   d\nh = e";
        let result = parse_program(input);
        assert!(result.is_ok());
        if let Ok(decls) = result {
            assert_eq!(decls.len(), 3);
        }
    }

    #[test]
    fn test_parentheses_with_semicolons() {
        // Parentheses changing precedence with semicolons
        let input = "f = (a ; b) ;; (c ; d)";
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_complex_decorator_nesting() {
        // Nested decorators with various argument types
        let input = r#"
            [optimize(3)]
            [inline]
            [pure(1, "mode")]
            [debug(f[x y])]
            foo [x: nat] : * = bar[x]
        "#;
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_adjacent_operators() {
        // Testing that operators can be adjacent without spaces
        let input = "f=a;b;;c;;;d;4;e";
        let result = parse_program(input);
        assert!(result.is_ok());
    }

    // ============================================================================
    // Negative tests - these should fail
    // ============================================================================

    #[test]
    fn test_invalid_ident_starts_with_number() {
        // Identifiers cannot start with a number
        let input = "123abc = x";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_invalid_ident_starts_with_hyphen() {
        // Identifiers cannot start with a hyphen
        let input = "-foo = x";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_invalid_ident_starts_with_dot() {
        // Identifiers cannot start with a dot
        let input = ".foo = x";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_invalid_ident_starts_with_apostrophe() {
        // Identifiers cannot start with an apostrophe
        let input = "'foo = x";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_unmatched_open_paren() {
        // Unmatched opening parenthesis
        let input = "f = (a b";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_unmatched_close_paren() {
        // Unmatched closing parenthesis
        let input = "f = a b)";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_unmatched_open_bracket() {
        // Unmatched opening bracket in args
        let input = "f = g[a b";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_unmatched_close_bracket() {
        // Unmatched closing bracket
        let input = "f = g a]";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_incomplete_decorator() {
        // Decorator without closing bracket
        let input = "[inline foo = bar";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_incomplete_semicolon_operator() {
        // ;N; format missing closing semicolon
        let input = "f = a ;3 b";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_invalid_literal_float_format() {
        // Invalid float format (two dots)
        let input = "f = g[1.2.3]";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_unterminated_string() {
        // String literal without closing quote
        let input = r#"f = g["hello]"#;
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_empty_input_is_ok() {
        // Empty input should be valid (zero declarations)
        let input = "";
        let result = parse_program(input);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 0);
    }

    #[test]
    fn test_only_whitespace_is_ok() {
        // Only whitespace should be valid
        let input = "   \n\n\t  \n  ";
        let result = parse_program(input);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 0);
    }

    #[test]
    fn test_declaration_without_equals_or_type() {
        // Declaration with just a name (no type, no body)
        let input = "foo";
        let result = parse_program(input);
        // This should actually succeed - it's a valid type-less, body-less declaration
        assert!(result.is_ok());
        let decls = result.unwrap();
        let expected = vec![Decl {
            decos: vec![],
            names: vec!["foo".to_string()],
            args: vec![],
            ty: None,
            body: None,
        }];
        assert_eq!(decls, expected);
    }

    #[test]
    fn test_multiple_equals() {
        // Multiple equals signs
        let input = "f = a = b";
        let result = parse_program(input);
        // This might actually parse as "f = a" with "= b" remaining
        assert!(result.is_err());
    }

    #[test]
    fn test_bracket_in_wrong_position() {
        // Brackets for args when there are multiple names
        let input = "f g [x: nat] = h";
        let result = parse_program(input);
        // Multiple names cannot have bracketed args
        assert!(result.is_err());
    }

    #[test]
    fn test_arrow_without_sides() {
        // Arrow operator without both sides
        let input = "f: → x";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_semicolon_without_operands() {
        // Semicolon at the start
        let input = "f = ; a";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_trailing_semicolon() {
        // Semicolon at the end with no right operand
        let input = "f = a ;";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_invalid_char_in_identifier() {
        // Special characters not allowed in identifiers
        let input = "foo@bar = x";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_comma_outside_list() {
        // Comma used outside of argument list
        let input = "f = a, b";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_mismatched_nested_parens() {
        // Mismatched nested parentheses
        let input = "f = ((a b) c";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_decorator_with_unclosed_args() {
        // Decorator with unclosed argument list
        let input = "[transform(f g] foo = bar";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_partial_arrow() {
        // Just an arrow without context
        let input = "→";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_double_colon() {
        // Double colon (not supported)
        let input = "f:: x";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_negative_without_number() {
        // Minus sign without a number following
        let input = "f = g[-]";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_chr_type_parsing() {
        // Debug: parse just the chr type declaration
        let input = "chr: x m x; x m → x (x m; m)";
        let result = decl(input);
        assert!(result.is_ok());
        let (remaining, d) = result.unwrap();
        eprintln!("Remaining input: {:?}", remaining);
        eprintln!("Parsed decl: names={:?}, has_body={}", d.names, d.body.is_some());
        assert_eq!(d.names, vec!["chr"]);
        assert!(d.body.is_none());
        assert_eq!(remaining, "");
    }

    #[test]
    fn test_chr_type_with_newline() {
        // Debug: parse chr type with newline after
        let input = "chr: x m x; x m → x (x m; m)\n";
        let result = decl(input);
        assert!(result.is_ok());
        let (remaining, d) = result.unwrap();
        eprintln!("Remaining input: {:?}", remaining);
        eprintln!("Parsed decl: names={:?}, has_body={}, body={:?}", d.names, d.body.is_some(), d.body);
        assert_eq!(d.names, vec!["chr"]);
        assert!(d.body.is_none());
    }

    #[test]
    fn test_chr_followed_by_decl() {
        // This is the problematic case from pentagon
        let input = "chr: x m x; x m → x (x m; m)\naaa = test";
        let result = parse_program(input);
        assert!(result.is_ok());
        let decls = result.unwrap();
        eprintln!("Parsed {} declarations", decls.len());
        for (i, decl) in decls.iter().enumerate() {
            eprintln!("  decl[{}]: names={:?}, has_body={}", i, decl.names, decl.body.is_some());
        }
        assert_eq!(decls.len(), 2);
        assert_eq!(decls[0].names, vec!["chr"]);
        assert!(decls[0].body.is_none()); // chr should NOT have a body
        assert_eq!(decls[1].names, vec!["aaa"]);
        assert!(decls[1].body.is_some()); // aaa should have a body
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
        assert!(result.is_ok());
        let decls = result.unwrap();

        // Structure check
        assert_eq!(decls.len(), 13);

        // u: *
        assert_eq!(decls[0], Decl {
            decos: vec![],
            names: vec!["u".to_string()],
            args: vec![],
            ty: Some(CellType::Star),
            body: None,
        });

        // x: u → u
        assert_eq!(decls[1], Decl {
            decos: vec![],
            names: vec!["x".to_string()],
            args: vec![],
            ty: Some(CellType::Arr(
                Cell::Var("u".to_string(), vec![]),
                Cell::Var("u".to_string(), vec![]),
            )),
            body: None,
        });

        // m: x x → x
        assert_eq!(decls[2], Decl {
            decos: vec![],
            names: vec!["m".to_string()],
            args: vec![],
            ty: Some(CellType::Arr(
                Cell::Comp(0, vec![
                    Cell::Var("x".to_string(), vec![]),
                    Cell::Var("x".to_string(), vec![]),
                ]),
                Cell::Var("x".to_string(), vec![]),
            )),
            body: None,
        });

        // a: m x; m → x m; m
        assert_eq!(decls[3], Decl {
            decos: vec![],
            names: vec!["a".to_string()],
            args: vec![],
            ty: Some(CellType::Arr(
                Cell::Comp(1, vec![
                    Cell::Comp(0, vec![
                        Cell::Var("m".to_string(), vec![]),
                        Cell::Var("x".to_string(), vec![]),
                    ]),
                    Cell::Var("m".to_string(), vec![]),
                ]),
                Cell::Comp(1, vec![
                    Cell::Comp(0, vec![
                        Cell::Var("x".to_string(), vec![]),
                        Cell::Var("m".to_string(), vec![]),
                    ]),
                    Cell::Var("m".to_string(), vec![]),
                ]),
            )),
            body: None,
        });

        // chl: (x m; m) x → x m x; m x
        assert_eq!(decls[4], Decl {
            decos: vec![],
            names: vec!["chl".to_string()],
            args: vec![],
            ty: Some(CellType::Arr(
                Cell::Comp(0, vec![
                    Cell::Comp(1, vec![
                        Cell::Comp(0, vec![
                            Cell::Var("x".to_string(), vec![]),
                            Cell::Var("m".to_string(), vec![]),
                        ]),
                        Cell::Var("m".to_string(), vec![]),
                    ]),
                    Cell::Var("x".to_string(), vec![]),
                ]),
                Cell::Comp(1, vec![
                    Cell::Comp(0, vec![
                        Cell::Var("x".to_string(), vec![]),
                        Cell::Comp(0, vec![
                            Cell::Var("m".to_string(), vec![]),
                            Cell::Var("x".to_string(), vec![]),
                        ]),
                    ]),
                    Cell::Comp(0, vec![
                        Cell::Var("m".to_string(), vec![]),
                        Cell::Var("x".to_string(), vec![]),
                    ]),
                ]),
            )),
            body: None,
        });

        // chr: x m x; x m → x (m x; m)
        assert_eq!(decls[5], Decl {
            decos: vec![],
            names: vec!["chr".to_string()],
            args: vec![],
            ty: Some(CellType::Arr(
                Cell::Comp(1, vec![
                    Cell::Comp(0, vec![
                        Cell::Var("x".to_string(), vec![]),
                        Cell::Comp(0, vec![
                            Cell::Var("m".to_string(), vec![]),
                            Cell::Var("x".to_string(), vec![]),
                        ]),
                    ]),
                    Cell::Comp(0, vec![
                        Cell::Var("x".to_string(), vec![]),
                        Cell::Var("m".to_string(), vec![]),
                    ]),
                ]),
                Cell::Comp(0, vec![
                    Cell::Var("x".to_string(), vec![]),
                    Cell::Comp(1, vec![
                        Cell::Comp(0, vec![
                            Cell::Var("m".to_string(), vec![]),
                            Cell::Var("x".to_string(), vec![]),
                        ]),
                        Cell::Var("m".to_string(), vec![]),
                    ]),
                ]),
            )),
            body: None,
        });

        // aaa = a x; m ;; chl; m ;; x m x; a ;; chr; m ;; x a; m
        assert_eq!(decls[6], Decl {
            decos: vec![],
            names: vec!["aaa".to_string()],
            args: vec![],
            ty: None,
            body: Some(Cell::Comp(2, vec![
                Cell::Comp(1, vec![
                    Cell::Comp(0, vec![
                        Cell::Var("a".to_string(), vec![]),
                        Cell::Var("x".to_string(), vec![]),
                    ]),
                    Cell::Var("m".to_string(), vec![]),
                ]),
                Cell::Comp(2, vec![
                    Cell::Comp(1, vec![
                        Cell::Var("chl".to_string(), vec![]),
                        Cell::Var("m".to_string(), vec![]),
                    ]),
                    Cell::Comp(2, vec![
                        Cell::Comp(1, vec![
                            Cell::Comp(0, vec![
                                Cell::Var("x".to_string(), vec![]),
                                Cell::Comp(0, vec![
                                    Cell::Var("m".to_string(), vec![]),
                                    Cell::Var("x".to_string(), vec![]),
                                ]),
                            ]),
                            Cell::Var("a".to_string(), vec![]),
                        ]),
                        Cell::Comp(2, vec![
                            Cell::Comp(1, vec![
                                Cell::Var("chr".to_string(), vec![]),
                                Cell::Var("m".to_string(), vec![]),
                            ]),
                            Cell::Comp(1, vec![
                                Cell::Comp(0, vec![
                                    Cell::Var("x".to_string(), vec![]),
                                    Cell::Var("a".to_string(), vec![]),
                                ]),
                                Cell::Var("m".to_string(), vec![]),
                            ]),
                        ]),
                    ]),
                ]),
            ])),
        });

        // ch0: m x x; x m → m m
        assert_eq!(decls[7], Decl {
            decos: vec![],
            names: vec!["ch0".to_string()],
            args: vec![],
            ty: Some(CellType::Arr(
                Cell::Comp(1, vec![
                    Cell::Comp(0, vec![
                        Cell::Var("m".to_string(), vec![]),
                        Cell::Comp(0, vec![
                            Cell::Var("x".to_string(), vec![]),
                            Cell::Var("x".to_string(), vec![]),
                        ]),
                    ]),
                    Cell::Comp(0, vec![
                        Cell::Var("x".to_string(), vec![]),
                        Cell::Var("m".to_string(), vec![]),
                    ]),
                ]),
                Cell::Comp(0, vec![
                    Cell::Var("m".to_string(), vec![]),
                    Cell::Var("m".to_string(), vec![]),
                ]),
            )),
            body: None,
        });

        // ch1: m m → x x m; m x
        assert_eq!(decls[8], Decl {
            decos: vec![],
            names: vec!["ch1".to_string()],
            args: vec![],
            ty: Some(CellType::Arr(
                Cell::Comp(0, vec![
                    Cell::Var("m".to_string(), vec![]),
                    Cell::Var("m".to_string(), vec![]),
                ]),
                Cell::Comp(1, vec![
                    Cell::Comp(0, vec![
                        Cell::Var("x".to_string(), vec![]),
                        Cell::Comp(0, vec![
                            Cell::Var("x".to_string(), vec![]),
                            Cell::Var("m".to_string(), vec![]),
                        ]),
                    ]),
                    Cell::Comp(0, vec![
                        Cell::Var("m".to_string(), vec![]),
                        Cell::Var("x".to_string(), vec![]),
                    ]),
                ]),
            )),
            body: None,
        });

        // kl: (m x; m) x → m x x; m x
        assert_eq!(decls[9], Decl {
            decos: vec![],
            names: vec!["kl".to_string()],
            args: vec![],
            ty: Some(CellType::Arr(
                Cell::Comp(0, vec![
                    Cell::Comp(1, vec![
                        Cell::Comp(0, vec![
                            Cell::Var("m".to_string(), vec![]),
                            Cell::Var("x".to_string(), vec![]),
                        ]),
                        Cell::Var("m".to_string(), vec![]),
                    ]),
                    Cell::Var("x".to_string(), vec![]),
                ]),
                Cell::Comp(1, vec![
                    Cell::Comp(0, vec![
                        Cell::Var("m".to_string(), vec![]),
                        Cell::Comp(0, vec![
                            Cell::Var("x".to_string(), vec![]),
                            Cell::Var("x".to_string(), vec![]),
                        ]),
                    ]),
                    Cell::Comp(0, vec![
                        Cell::Var("m".to_string(), vec![]),
                        Cell::Var("x".to_string(), vec![]),
                    ]),
                ]),
            )),
            body: None,
        });

        // kr: x x m; x m → x (x m; m)
        assert_eq!(decls[10], Decl {
            decos: vec![],
            names: vec!["kr".to_string()],
            args: vec![],
            ty: Some(CellType::Arr(
                Cell::Comp(1, vec![
                    Cell::Comp(0, vec![
                        Cell::Var("x".to_string(), vec![]),
                        Cell::Comp(0, vec![
                            Cell::Var("x".to_string(), vec![]),
                            Cell::Var("m".to_string(), vec![]),
                        ]),
                    ]),
                    Cell::Comp(0, vec![
                        Cell::Var("x".to_string(), vec![]),
                        Cell::Var("m".to_string(), vec![]),
                    ]),
                ]),
                Cell::Comp(0, vec![
                    Cell::Var("x".to_string(), vec![]),
                    Cell::Comp(1, vec![
                        Cell::Comp(0, vec![
                            Cell::Var("x".to_string(), vec![]),
                            Cell::Var("m".to_string(), vec![]),
                        ]),
                        Cell::Var("m".to_string(), vec![]),
                    ]),
                ]),
            )),
            body: None,
        });

        // oao = kl; m ;; m x x; a ;; (ch0 ;; ch1); m ;; x x m; a ;; kr; m
        assert_eq!(decls[11], Decl {
            decos: vec![],
            names: vec!["oao".to_string()],
            args: vec![],
            ty: None,
            body: Some(Cell::Comp(2, vec![
                Cell::Comp(1, vec![
                    Cell::Var("kl".to_string(), vec![]),
                    Cell::Var("m".to_string(), vec![]),
                ]),
                Cell::Comp(2, vec![
                    Cell::Comp(1, vec![
                        Cell::Comp(0, vec![
                            Cell::Var("m".to_string(), vec![]),
                            Cell::Comp(0, vec![
                                Cell::Var("x".to_string(), vec![]),
                                Cell::Var("x".to_string(), vec![]),
                            ]),
                        ]),
                        Cell::Var("a".to_string(), vec![]),
                    ]),
                    Cell::Comp(2, vec![
                        Cell::Comp(1, vec![
                            Cell::Comp(2, vec![
                                Cell::Var("ch0".to_string(), vec![]),
                                Cell::Var("ch1".to_string(), vec![]),
                            ]),
                            Cell::Var("m".to_string(), vec![]),
                        ]),
                        Cell::Comp(2, vec![
                            Cell::Comp(1, vec![
                                Cell::Comp(0, vec![
                                    Cell::Var("x".to_string(), vec![]),
                                    Cell::Comp(0, vec![
                                        Cell::Var("x".to_string(), vec![]),
                                        Cell::Var("m".to_string(), vec![]),
                                    ]),
                                ]),
                                Cell::Var("a".to_string(), vec![]),
                            ]),
                            Cell::Comp(1, vec![
                                Cell::Var("kr".to_string(), vec![]),
                                Cell::Var("m".to_string(), vec![]),
                            ]),
                        ]),
                    ]),
                ]),
            ])),
        });

        // pentagon: aaa → oao
        assert_eq!(decls[12], Decl {
            decos: vec![],
            names: vec!["pentagon".to_string()],
            args: vec![],
            ty: Some(CellType::Arr(
                Cell::Var("aaa".to_string(), vec![]),
                Cell::Var("oao".to_string(), vec![]),
            )),
            body: None,
        });
    }
}
