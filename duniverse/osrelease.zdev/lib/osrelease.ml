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

open Rresult
open Astring
open Bos
open Sexplib.Conv

let uname f =
  let cmd = Cmd.(v "uname" % f) in
  OS.Cmd.(run_out cmd |> to_string |> R.get_ok)

module Arch = struct
  type t =
    [ `X86_32
    | `X86_64
    | `Ppc32
    | `Ppc64 of [ `Le | `Be ]
    | `Arm32 of [ `Armv5 | `Armv6 | `Earmv6 | `Armv7 | `Earmv7 ]
    | `Aarch64
    | `Unknown of string ]
  [@@deriving sexp]

  let to_string (x : t) =
    match x with
    | `X86_32 -> "x86_32"
    | `X86_64 -> "x86_64"
    | `Ppc32 -> "ppc32"
    | `Ppc64 `Be -> "ppc64"
    | `Ppc64 `Le -> "ppc64le"
    | `Arm32 _ -> "arm32"
    | `Aarch64 -> "arm64"
    | `Unknown v -> v

  let pp fmt v = Format.pp_print_string fmt (to_string v)

  let of_string v : t =
    match String.Ascii.lowercase v with
    | "x86" | "i386" | "i586" | "i686" -> `X86_32
    | "powerpc" | "ppc" | "ppcle" -> `Ppc32
    | "amd64" | "x86_64" -> `X86_64
    | "ppc64" | "ppc64be" -> `Ppc64 `Be
    | "ppc64le" -> `Ppc64 `Le
    | "aarch64_be" | "aarch64" | "armv8b" | "armv8l" -> `Aarch64
    | "armv5" -> `Arm32 `Armv5
    | "armv6" -> `Arm32 `Armv6
    | "earmv6" -> `Arm32 `Earmv6
    | "armv7" -> `Arm32 `Earmv7
    | v -> `Unknown v

  let v () =
    match Sys.os_type with
    | "Unix" | "Cygwin" -> uname "-m" |> of_string
    | "Win32" when Sys.word_size = 32 (* TODO WoW64? *) -> `X86_32
    | "Win32" -> `X86_64
    | v -> `Unknown v
end

module OS = struct
  type t =
    [ `Linux
    | `MacOS
    | `Win32
    | `Cygwin
    | `FreeBSD
    | `OpenBSD
    | `DragonFly
    | `Unknown of string ]
  [@@deriving sexp]

  let to_string (v : t) =
    match v with
    | `Linux -> "linux"
    | `MacOS -> "macos"
    | `Win32 -> "win32"
    | `Cygwin -> "cygwin"
    | `FreeBSD -> "freebsd"
    | `OpenBSD -> "openbsd"
    | `DragonFly -> "dragonfly"
    | `Unknown v -> v

  let of_string v : t =
    match String.Ascii.lowercase v with
    | "darwin" | "osx" -> `MacOS
    | "linux" -> `Linux
    | "win32" -> `Win32
    | "cygwin" -> `Cygwin
    | "freebsd" -> `FreeBSD
    | "openbsd" -> `OpenBSD
    | "dragonfly" -> `DragonFly
    | v -> `Unknown v

  let pp fmt v = Format.pp_print_string fmt (to_string v)

  let v () =
    match Sys.os_type with
    | "Unix" -> uname "-s" |> of_string
    | v -> of_string v
end

module Distro = struct
  type linux =
    [ `Arch
    | `Alpine
    | `CentOS
    | `Debian
    | `Fedora
    | `Gentoo
    | `Mageia
    | `NixOS
    | `OracleLinux
    | `RHEL
    | `Ubuntu
    | `OpenSUSE
    | `Android
    | `Other of string ]
  [@@deriving sexp]

  type macos = [ `Homebrew | `MacPorts | `None ] [@@deriving sexp]

  type windows = [ `Cygwin | `None ] [@@deriving sexp]

  type t =
    [ `Linux of linux
    | `MacOS of macos
    | `Windows of windows
    | `Other of string ]
  [@@deriving sexp]

  let linux_to_string (x : linux) =
    match x with
    | `Alpine -> "alpine"
    | `Android -> "android"
    | `Arch -> "arch"
    | `CentOS -> "centos"
    | `Debian -> "debian"
    | `Fedora -> "fedora"
    | `Gentoo -> "gentoo"
    | `Mageia -> "mageia"
    | `NixOS -> "nixos"
    | `OpenSUSE -> "opensuse"
    | `OracleLinux -> "oraclelinux"
    | `Other v -> v
    | `RHEL -> "rhel"
    | `Ubuntu -> "ubuntu"

  let macos_to_string (x : macos) =
    match x with
    | `Homebrew -> "homebrew"
    | `MacPorts -> "macports"
    | `None -> "macos"

  let windows_to_string (x : windows) =
    match x with `Cygwin -> "cygwin" | `None -> "windows"

  let to_string (x : t) =
    match x with
    | `Linux v -> linux_to_string v
    | `MacOS v -> macos_to_string v
    | `Other v -> v
    | `Windows v -> windows_to_string v

  let pp fmt v = Format.pp_print_string fmt (to_string v)

  let android_release =
    lazy
      (let open Bos in
      let cmd = Cmd.(v "getprop" % "ro.build.version.release") in
      match OS.Cmd.(run_out cmd |> to_string) with
      | Ok "" -> None
      | Ok out -> Some out
      | Error _ -> None)

  let find_first_file files = List.find_opt Sys.file_exists files

  let os_release_fields =
    lazy
      (let os_release_file =
         find_first_file [ "/etc/os-release"; "/usr/lib/os-release" ]
       in
       match os_release_file with
       | None -> Error (`Msg "no os-release file found")
       | Some file ->
           Bos.OS.File.fold_lines
             (fun acc line ->
               let open Scanf in
               try
                 sscanf line "%s@= %s" (fun k v ->
                     try sscanf v "\"%s@\"" (fun s -> (k, s) :: acc)
                     with Scan_failure _ | End_of_file -> acc)
               with Scan_failure _ | End_of_file -> acc)
             [] (Fpath.v file))

  let os_release_field f =
    Lazy.force os_release_fields >>| List.assoc_opt f >>| function
    | Some "" -> None
    | v -> v

  let identify_linux () =
    os_release_field "ID" >>= function
    | Some v -> Ok (Some v)
    | None -> (
        let cmd = Cmd.(v "lsb_release" % "-i" % "-s") in
        Bos.OS.Cmd.(run_out cmd |> to_string) |> function
        | Ok v -> Ok (Some (String.Ascii.lowercase v))
        | Error _ -> (
            let issue =
              find_first_file
                [
                  "/etc/redhat-release";
                  "/etc/centos-release";
                  "/etc/gentoo-release";
                  "/etc/issue";
                ]
            in
            match issue with
            | None -> Ok None
            | Some f -> (
                Bos.OS.File.read (Fpath.v f) >>= fun v ->
                match Scanf.sscanf v " %s " (fun x -> x) with
                | "" -> Ok None
                | v -> Ok (Some (String.Ascii.lowercase v))
                | exception Not_found -> Ok None ) ) )

  let v () : (t, [ `Msg of string ]) result =
    match OS.v () with
    | `MacOS -> (
        Bos.OS.Cmd.exists (Cmd.v "brew") >>= function
        | true -> Ok (`MacOS `Homebrew)
        | false -> (
            Bos.OS.Cmd.exists (Cmd.v "port") >>= function
            | true -> Ok (`MacOS `MacPorts)
            | false -> Ok (`MacOS `None) ) )
    | `Linux -> (
        match Lazy.force android_release with
        | Some _ -> Ok (`Linux `Android)
        | None -> (
            identify_linux () >>= function
            | None -> Ok (`Linux (`Other ""))
            | Some v -> Ok (`Linux (`Other v)) ) )
    | _ -> Error (`Msg "foo")
end

module Version = struct
  let detect_linux_version () =
    match Lazy.force Distro.android_release with
    | Some r -> Ok (Some r)
    | None -> (
        let cmd = Cmd.(v "lsb_release" % "-r" % "-s") in
        Bos.OS.Cmd.(run_out cmd |> to_string) |> function
        | Ok v -> Ok (Some v)
        | Error _ -> Distro.os_release_field "VERSION_ID" )

  let detect_macos_version () =
    let cmd = Cmd.(v "sw_vers" % "-productVersion") in
    Bos.OS.Cmd.(run_out cmd |> to_string) >>| function
    | "" -> None
    | v -> Some v

  let detect_freebsd_version () =
    let cmd = Cmd.(v "uname" % "-U") in
    Bos.OS.Cmd.(run_out cmd |> to_string) >>| function
    | "" -> None
    | v -> Some v

  let v () =
    match OS.v () with
    | `Linux -> detect_linux_version ()
    | `MacOS -> detect_macos_version ()
    | `FreeBSD -> detect_freebsd_version ()
    | `Win32 | `OpenBSD | `DragonFly | `Cygwin ->
        Error (`Msg "Version detection on this platform not yet supported")
    | `Unknown _ -> Ok None
end
