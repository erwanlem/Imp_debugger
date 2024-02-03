{

  open Lexing
  open Impparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    (* Liste des mots-clés : À COMPLÉTER *)
    [ "print",    PRINT;
      "function", FUNCTION;
      "if", IF;
      "else", ELSE;
      "true", BOOL (true);
      "false", BOOL (false);
      "var", VAR;
      "return", RETURN;
      "while", WHILE;
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  

(* Règles : À COMPLÉTER *)
rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf; token lexbuf }

  
  | number as n  { INT(int_of_string n) }
  | ident as id  { keyword_or_ident id }

  | ";"  { SEMI }
  | ","  { COMMA }
  | "+"  { PLUS }
  | "-"  { SUB }
  | "*"  { STAR }
  | "/"  { DIV }
  | "["  { LB }
  | "]"  { RB }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | "==" { EQ }
  | "="  { EQUAL }
  | "%"  { REM }
  | "<=" { LE }
  | "<"  { LT }
  | ">=" { GE }
  | ">"  { GT }
  | "!=" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | "!"  { NOT }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
