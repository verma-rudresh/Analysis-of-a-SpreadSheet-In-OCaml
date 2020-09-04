(* File lexer.mll *)
{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof
}

let ws = [' ''\t']
let digits = ['0'-'9']+
let digits_float = ['-']?['0'-'9']+['.']*['0'-'9']*

rule token = parse
| ['\n' ]                           { token lexbuf }

| digits_float as d                       { CONST(float_of_string d) }

| '[' [' ']* (digits as d1) [' ']* ',' [' ']* (digits as d2) [' ']* ']'   
{ INDEX(int_of_string d1,int_of_string d2); }


| '(' [' ']* ('[' [' ']* (digits as p1) [' ']* ',' [' ']* (digits as p2) [' ']* ']') [' ']* ':' [' ']* ('[' [' ']* (digits as d1) [' ']* ',' [' ']* (digits as d2) [' ']* ']') [' ']* ')'                                      
{ RANGE(int_of_string p1,int_of_string p2,int_of_string d1,int_of_string d2) }

| ':''='                            { ASSIGN }
| ';'                               { SEMICOLON }
| "SUM"                             { SUM }
| "AVG"                             { AVG }
| "MIN"                             { MIN }
| "MAX"                             { MAX }
| "ADD"                             { ADD }
| "SUBT"                            { SUBT }
| "MULT"                            { MULT }
| "DIV"                             { DIV }
| "COUNT"                           { COUNT }
| "ROWCOUNT"                        { ROWCOUNT }
| "COLCOUNT"                        { COLCOUNT }
| "ROWAVG"                          { ROWAVG }
| "COLAVG"                          { COLAVG }
| "ROWSUM"                          { ROWSUM }
| "COLSUM"                          { COLSUM }
| "ROWMAX"                          { ROWMAX }
| "ROWMIN"                          { ROWMIN }
| "COLMIN"                          { COLMIN }
| "COLMAX"                          { COLMAX }
| [' ' '\t']                        { token lexbuf }
| eof                               { raise Eof }
