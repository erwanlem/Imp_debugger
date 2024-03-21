%{

  open Imp

%}

(* Liste de lexèmes *)
(* À COMPLÉTER *)
%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token PLUS STAR DIV SUB EQUAL REM
%token LT LE GT GE EQ NEQ
%token AND OR
%token NOT
%token LPAR RPAR LB RB BEGIN END
%token IF ELSE
%token VAR
%token SEMI COMMA
%token WHILE
%token FUNCTION PRINT RETURN
%token EOF

(* Priorités *)
(* À COMPLÉTER *)
%left OR
%left AND
%right NOT
%nonassoc LT LE GT GE EQ NEQ
%left PLUS SUB
%left STAR DIV REM
%nonassoc LB

(* Point d'entrée *)
%start program
%type <Imp.program> program

%%

(* Règles *)

program:
| decl=list(var_decl) affect=list(instr) functions=list(fun_def) EOF 
    { {globals=decl
    ; functions; main=List.find (fun f -> f.name = "main") functions } }
;

fun_def:
| FUNCTION name=IDENT LPAR param=separated_list(COMMA, IDENT) RPAR
    BEGIN decl=list(var_decl) code=list(instr) END
    { {name; code; params=List.fold_left (fun acc id -> id :: acc) [] param; 
    locals=decl; id=instruction_id ()} }
;

var_decl:
  | s=simple_var_decl SEMI { s }
  | VAR id=IDENT EQUAL e=expr SEMI { (id, Some e, instruction_id ()) }
;

simple_var_decl:
  | VAR id=IDENT { (id, None, instruction_id ()) }
;


instr:
| PRINT LPAR e=expr RPAR SEMI                       { Print(e, instruction_id ()) }
| IF LPAR e=expr RPAR BEGIN i1=list(instr) END ELSE BEGIN i2=list(instr) END   { If(e, i1, i2, instruction_id ()) }
| IF LPAR e=expr RPAR BEGIN i1=list(instr) END      { If(e, i1, [], instruction_id ()) }
| var=expr EQUAL value=expr SEMI                    { match var with
                                                      | Var name -> Set (name, value, instruction_id ())
                                                      | GetArr(id, e) -> SetArr(id, e, value, instruction_id ()) 
                                                      | _ -> failwith "Invalid ID name" }
| e=expr SEMI                                       { Expr (e, instruction_id ()) }
| RETURN LPAR e=expr RPAR SEMI                      { Return (e, instruction_id ()) }
| WHILE LPAR cond=expr RPAR BEGIN i=list(instr) END { While(cond, i, instruction_id ()) }
;

expr:
| n=INT                                              { Int n }
| b=BOOL                                             { Bool b }
| LPAR e=expr RPAR                                   { e }
| e1=expr op=binop e2=expr                           { Binop(op, e1, e2) }
| id=IDENT                                           { Var id }
| op=unop e=expr                                     { Unop(op, e) }
| BEGIN l=list(terminated(expr, SEMI)) END           { Array l }
| e1=expr LB e=expr RB                              { GetArr(e1, e) }
| name=IDENT LPAR p=separated_list(COMMA, expr) RPAR { Call(name, p) }
;


%inline unop:
| SUB { Opp }
| NOT { Not }
;

%inline binop:
| PLUS { Add }
| STAR { Mul }
| SUB  { Sub }
| DIV  { Div }
| REM  { Rem }
| LT   { Lt }
| LE   { Le }
| GT   { Gt }
| GE   { Ge }
| EQ   { Eq }
| NEQ  { Neq }
| AND  { And }
| OR   { Or }
;
