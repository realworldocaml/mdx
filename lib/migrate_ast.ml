(*
 *  This file originates from OCamlFormat.
 *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.
 *
 *  This source code is licensed under the MIT license.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 *  in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

open Compat

module Ast_helper = Ppxlib.Ast_helper
module Asttypes = Ppxlib.Asttypes
module Parsetree = Ppxlib.Parsetree

module Parse = struct
  let toplevel_phrase = Ppxlib_ast.Parse.toplevel_phrase

  let implementation = Ppxlib_ast.Parse.implementation

  let interface = Ppxlib_ast.Parse.interface
end

module Position = struct
  open Lexing

  let column {pos_fname=_; pos_lnum=_; pos_bol; pos_cnum} = pos_cnum - pos_bol

  let fmt fs {pos_fname=_; pos_lnum; pos_bol; pos_cnum} =
    if pos_lnum = -1 then Format.fprintf fs "[%d]" pos_cnum
    else Format.fprintf fs "[%d,%d+%d]" pos_lnum pos_bol (pos_cnum - pos_bol)

  let compare_col p1 p2 = (column p1) - (column p2)

  let equal p1 p2 =
    String.equal p1.pos_fname p2.pos_fname
    && p1.pos_lnum = p2.pos_lnum
    && p1.pos_bol = p2.pos_bol
    && p1.pos_cnum = p2.pos_cnum

  let compare p1 p2 =
    if equal p1 p2 then 0 else p1.pos_cnum - p2.pos_cnum

  let distance p1 p2 = p2.pos_cnum - p1.pos_cnum
end

module Location = struct
  include Ppxlib.Location

  let fmt fs {loc_start; loc_end; loc_ghost} =
    Format.fprintf fs "(%a..%a)%s" Position.fmt loc_start Position.fmt
      loc_end
      (if loc_ghost then " ghost" else "")

  let to_string x = Format.asprintf "%a" fmt x

  let compare x y =
    let compare_start = Position.compare x.loc_start y.loc_start in
    if compare_start = 0 then Position.compare x.loc_end y.loc_end
    else compare_start

  let hash = Hashtbl.hash

  let is_single_line x = x.loc_start.pos_lnum = x.loc_end.pos_lnum

  let compare_start x y = Position.compare x.loc_start y.loc_start

  let compare_start_col x y = Position.compare_col x.loc_start y.loc_start

  let compare_end x y = Position.compare x.loc_end y.loc_end

  let compare_end_col x y = Position.compare_col x.loc_end y.loc_end

  let contains l1 l2 = compare_start l1 l2 <= 0 && compare_end l1 l2 >= 0

  let width x = Position.distance x.loc_start x.loc_end

  let compare_width_decreasing l1 l2 = (width l2) - (width l1)

  let mkloc = Location.mkloc

  let mknoloc = Location.mknoloc

  let formatter_for_warnings = Location.formatter_for_warnings
end
