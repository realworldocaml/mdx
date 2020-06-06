(* 
 * Copyright (c) 2020 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

module Arch : sig
  type t =
    [ `Aarch64
    | `Arm32 of [ `Armv5 | `Armv6 | `Armv7 | `Earmv6 | `Earmv7 ]
    | `Ppc32
    | `Ppc64 of [ `Be | `Le ]
    | `Unknown of string
    | `X86_32
    | `X86_64 ]
  [@@deriving sexp]

  val to_string : t -> string

  val of_string : string -> t

  val pp : Format.formatter -> t -> unit

  val v : unit -> t
end

module OS : sig
  type t =
    [ `Cygwin
    | `DragonFly
    | `FreeBSD
    | `Linux
    | `MacOS
    | `OpenBSD
    | `Unknown of string
    | `Win32 ]
  [@@deriving sexp]

  val to_string : t -> string

  val of_string : string -> t

  val pp : Format.formatter -> t -> unit

  val v : unit -> t
end

module Distro : sig
  type linux =
    [ `Alpine
    | `Android
    | `Arch
    | `CentOS
    | `Debian
    | `Fedora
    | `Gentoo
    | `Mageia
    | `NixOS
    | `OpenSUSE
    | `OracleLinux
    | `Other of string
    | `RHEL
    | `Ubuntu ]
  [@@deriving sexp]

  type macos = [ `Homebrew | `MacPorts | `None ] [@@deriving sexp]

  type windows = [ `Cygwin | `None ] [@@deriving sexp]

  type t =
    [ `Linux of linux
    | `MacOS of macos
    | `Other of string
    | `Windows of windows ]
  [@@deriving sexp]

  val linux_to_string : linux -> string

  val macos_to_string : macos -> string

  val windows_to_string : windows -> string

  val to_string : t -> string

  val pp : Format.formatter -> t -> unit

  val os_release_field : string -> (string option, [> Rresult.R.msg ]) result

  val v : unit -> (t, [ `Msg of string ]) result
end

module Version : sig
  val v : unit -> (string option, [> Rresult.R.msg ]) result
end
