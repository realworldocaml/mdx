(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Results

    Abbridged  [rresult]. See {!section:Topkg.prels} for documention. *)

val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
val ( >>| ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result

type ('a, 'b) r = ('a, 'b) result = Ok of 'a | Error of 'b
type 'a result = ('a, [ `Msg of string]) r

module R : sig
  val reword_error : ('b -> 'c) -> ('a, 'b) r -> ('a, 'c) r

  type msg = [ `Msg of string ]

  val msgf : ('a, Format.formatter, unit, [> msg]) format4 -> 'a

  val error_msg : string -> ('b, [> msg]) r
  val error_msgf :
    ('a, Format.formatter, unit, ('b, [> msg]) r) format4 -> 'a

  val reword_error_msg :
    ?replace:bool -> (string -> msg) -> ('a, msg) r -> ('a, [> msg]) r
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
