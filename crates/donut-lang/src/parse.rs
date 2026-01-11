use crate::types;
use nom::{IResult, Parser};

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
