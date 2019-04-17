(* Copyright (c) 2018 Anil Madhavapeddy <anil@recoil.org>
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

val map : ('a -> ('b, 'c) result) -> 'a list -> ('b list, 'c) result

val iter : ('a -> (unit, 'b) result) -> 'a list -> (unit, 'b) result

val git_default_branch : remote:string -> unit -> (string, [> Rresult.R.msg ]) result
(** Return the default branch for the given remote name by running git remote show [remote] and
    parsing the output looking for HEAD branch: <branch_name> *)

val git_archive :
  output_dir:Fpath.t -> remote:string -> tag:string -> unit -> (unit, [> Rresult.R.msg ]) result
(** [git_archive ~output_dir ~remote ~tag] clones the repo from [remote] into [output_dir],
    checks it out to [tag] and removes its .git and duniverse directories. *)

val git_add_and_commit :
  repo:Fpath.t -> message:string -> Bos.Cmd.t -> (unit, [> Rresult.R.msg ]) result
(** [git_add_and_commit ~repo ~message files] adds [files] to [repo] and commits them with
    [message]. *)

val is_git_repo_clean : repo:Fpath.t -> unit -> (bool, [> Rresult.R.msg ]) result
(** Return whether the given repo is clean, ie return true if there is no uncommitted changes *)

val git_checkout : ?args:Bos.Cmd.t -> repo:Fpath.t -> string -> (unit, [> Rresult.R.msg ]) result
(** [git_checkout ~args ~repo branch] checks out the git repository in [repo] to branch [branch]
    with the extra arguments [args]. *)

val git_checkout_or_branch : repo:Fpath.t -> string -> (unit, [> Rresult.R.msg ]) result
(** [git_checkout ~repo branch] checks out the git repository in [repo] to branch [branch] creating
    it if it doesn't exist yet. *)

val git_add_all_and_commit :
  repo:Fpath.t -> message:string -> unit -> (unit, [> Rresult.R.msg ]) result
(** [git_add_all_and_commit ~repo ~message ()] runs git add -am [message] in [repo]. *)

val git_merge :
  ?args:Bos.Cmd.t -> from:string -> repo:Fpath.t -> unit -> (unit, [> Rresult.R.msg ]) result
(** [git_merge ~args ~repo branch] merges [from] into [repo]'s current active branch with the extra
    arguments [args]. *)

val run_opam_package_deps : root:Fpath.t -> string list -> (string list, [> Rresult.R.msg ]) result
(** [run_opam_packages_deps ~root packages] returns a list of versioned constrained packages that
    resolves the transitive dependencies of [packages]. *)

module Opam_show_result : sig
  type t

  val make : root: Fpath.t -> string list -> Types.Opam.package list -> (t, [> Rresult.R.msg ]) result
  val get  : package: string -> field: string -> t -> OpamParserTypes.value option
end

(** [init_local_opam_switch ~opam_switch ~root ~remotes ()] creates a fresh opam state and switch
    with compiler [compiler] using [root] as OPAMROOT and adds the [remotes] opam repositories
    to it. *)

val init_opam_and_remotes :
  root:Fpath.t -> remotes:Types.Opam.Remote.t list -> unit -> (unit, [> Rresult.R.msg ]) result
(** [init_local_opam_switch ~root ~remotes ()] creates a fresh opam state with a single
    switch using [root] as OPAMROOT and adds the [remotes] opam repositories to it. *)

val add_opam_dev_pin : root:Fpath.t -> Types.Opam.pin -> (unit, [> Rresult.R.msg ]) result
(** [add_opam_dev_pin ~root pin] pins [pin] in the active switch using [root] as OPAMROOT. *)

val add_opam_local_pin : root:Fpath.t -> string -> (unit, [> Rresult.R.msg ]) result
(** [add_opam_local_pin ~root package] pins the package in the current working dir under
    [package ^ ".dev"] in the active switch using [root] as OPAMROOT. *)
