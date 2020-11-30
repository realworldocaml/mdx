/**************************************************************************/
/*                                                                        */
/*    Copyright 2012-2017 OCamlPro                                        */
/*    Copyright 2012 INRIA                                                */
/*                                                                        */
/*  All rights reserved. This file is distributed under the terms of the  */
/*  GNU Lesser General Public License version 2.1, with the special       */
/*  exception on linking described in the file LICENSE.                   */
/*                                                                        */
/**************************************************************************/

%{

open OpamParserTypes.FullPos

(** Opam config file generic type parser *)

let get_pos_full ?(s=1) n =
  let spos = Parsing.rhs_start_pos s in
  let epos = Parsing.rhs_end_pos n in
  Lexing.({
      filename = spos.pos_fname;
      start = spos.pos_lnum, spos.pos_cnum - spos.pos_bol;
      stop = epos.pos_lnum, epos.pos_cnum - epos.pos_bol;
    })

let get_pos n = get_pos_full ~s:n n

%}

%token <string> STRING IDENT
%token <bool> BOOL
%token EOF
%token LBRACKET RBRACKET
%token LPAR RPAR
%token LBRACE RBRACE
%token COLON
%token <int> INT
%token <OpamParserTypes.FullPos.relop_kind> RELOP
%token AND
%token OR
%token <OpamParserTypes.FullPos.pfxop_kind> PFXOP
%token <OpamParserTypes.FullPos.env_update_op_kind> ENVOP

%left COLON
%left ATOM
%left OR
%left AND
%nonassoc ENVOP
%nonassoc PFXOP
%left LBRACE RBRACE
%nonassoc RELOP
%nonassoc URELOP

%start main value
%type <string -> OpamParserTypes.FullPos.opamfile> main
%type <OpamParserTypes.FullPos.value> value
%type <OpamParserTypes.FullPos.value list> values
%type <OpamParserTypes.FullPos.opamfile_item> item

%%

main:
| items EOF { fun file_name ->
        { file_contents = $1; file_name } }
;

items:
| item items { $1 :: $2 }
|            { [] }
;

item:
| IDENT COLON value                {
  { pos = get_pos_full 3;
    pelem =
      Variable ({ pos = get_pos 1; pelem =  $1 }, $3);
  }
}
| IDENT LBRACE items RBRACE {
  { pos = get_pos_full 4;
    pelem =
      Section ({section_kind = { pos = get_pos 1; pelem = $1 };
                section_name = None;
                section_items =
                  { pos = get_pos_full ~s:2 4; pelem = $3 };
               })
  }
}
| IDENT STRING LBRACE items RBRACE {
  { pos = get_pos_full 4;
    pelem =
      Section ({section_kind = { pos = get_pos 1; pelem = $1 };
                section_name = Some { pos = get_pos 2; pelem = $2 };
                section_items =
                  { pos = get_pos_full ~s:2 4; pelem = $4 };
               })
  }
}
;

value:
| atom            %prec ATOM { $1 }
| LPAR values RPAR           {{ pos = get_pos_full 3 ; pelem = Group { pos = get_pos_full ~s:1 3; pelem = $2 } }}
| LBRACKET values RBRACKET   {{ pos = get_pos_full 3 ; pelem = List { pos = get_pos_full ~s:1 3; pelem = $2 } }}
| value LBRACE values RBRACE {{ pos = get_pos_full 4 ;
                                pelem = Option ($1, { pos = get_pos_full ~s:2 4; pelem = $3 }) }}
| value AND value            {{ pos = get_pos_full 3 ; pelem = Logop ({ pos = get_pos 2 ; pelem = `And },$1,$3) }}
| value OR value             {{ pos = get_pos_full 3 ; pelem = Logop ({ pos = get_pos 2 ; pelem = `Or },$1,$3) }}
| atom RELOP atom            {{ pos = get_pos_full 3 ; pelem = Relop ({ pos = get_pos 2 ; pelem = $2 },$1,$3) }}
| atom ENVOP atom            {{ pos = get_pos_full 3 ; pelem = Env_binding ($1,{ pos = get_pos 2 ; pelem = $2 },$3) }}
| PFXOP value                {{ pos = get_pos_full 2 ; pelem = Pfxop ({ pos = get_pos 1 ; pelem = $1 },$2) }}
| RELOP atom                 {{ pos = get_pos_full 2 ; pelem = Prefix_relop ({ pos = get_pos 1 ; pelem = $1 },$2) }}
;

values:
|                            { [] }
| value values               { $1 :: $2 }
;

atom:
| IDENT                      {{ pos = get_pos 1 ; pelem = Ident $1 }}
| BOOL                       {{ pos = get_pos 1 ; pelem = Bool $1 }}
| INT                        {{ pos = get_pos 1 ; pelem = Int $1 }}
| STRING                     {{ pos = get_pos 1 ; pelem = String $1 }}
;

%%

let main t l f =
  try
    let r = main t l f in
    Parsing.clear_parser ();
    r
  with
  | e ->
    Parsing.clear_parser ();
    raise e
