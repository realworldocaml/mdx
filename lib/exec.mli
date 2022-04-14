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
val opam_version : unit -> (string, [> Rresult.R.msg ]) result

val ocaml_version :
  ?ocamlc:Fpath.t -> unit -> (Ocaml_version.t, [> Rresult.R.msg ]) result

val dune_version : unit -> (string, [> Rresult.R.msg ]) result

val dune_build :
  root:Fpath.t ->
  ?profile:string ->
  string list ->
  (unit, [> Rresult.R.msg ]) result

val dune_install :
  root:Fpath.t ->
  prefix:Fpath.t ->
  sections:string list ->
  string list ->
  (unit, [> Rresult.R.msg ]) result

val install_ocaml_to :
  prefix:Fpath.t -> src:Fpath.t -> unit -> (unit, [> Rresult.R.msg ]) result

val install_dune_to :
  prefix:Fpath.t -> src:Fpath.t -> unit -> (unit, [> Rresult.R.msg ]) result

val git_default_branch :
  remote:string -> unit -> (string, [> Rresult.R.msg ]) result
(** Return the default branch for the given remote name by running git remote show [remote] and
    parsing the output looking for HEAD branch: <branch_name> *)

val git_shallow_clone :
  output_dir:Fpath.t ->
  remote:string ->
  ref:string ->
  unit ->
  (unit, [> Rresult.R.msg ]) result

val git_rev_parse :
  repo:Fpath.t -> ref:string -> unit -> (string, [> Rresult.R.msg ]) result

val git_unshallow : repo:Fpath.t -> unit -> (unit, [> Rresult.R.msg ]) result

val git_add_and_commit :
  repo:Fpath.t ->
  message:string ->
  Bos.Cmd.t ->
  (unit, [> Rresult.R.msg ]) result
(** [git_add_and_commit ~repo ~message files] adds [files] to [repo] and commits them with
    [message]. *)

val is_git_repo_clean :
  repo:Fpath.t -> unit -> (bool, [> Rresult.R.msg ]) result
(** Return whether the given repo is clean, ie return true if there is no uncommitted changes *)

val git_checkout :
  ?args:Bos.Cmd.t -> repo:Fpath.t -> string -> (unit, [> Rresult.R.msg ]) result
(** [git_checkout ~args ~repo branch] checks out the git repository in [repo] to branch [branch]
    with the extra arguments [args]. *)

val git_checkout_or_branch :
  repo:Fpath.t -> string -> (unit, [> Rresult.R.msg ]) result
(** [git_checkout ~repo branch] checks out the git repository in [repo] to branch [branch] creating
    it if it doesn't exist yet. *)

val git_add_all_and_commit :
  repo:Fpath.t -> message:string -> unit -> (unit, [> Rresult.R.msg ]) result
(** [git_add_all_and_commit ~repo ~message ()] runs git add -am [message] in [repo]. *)

val git_merge :
  ?args:Bos.Cmd.t ->
  from:string ->
  repo:Fpath.t ->
  unit ->
  (unit, [> Rresult.R.msg ]) result
(** [git_merge ~args ~repo branch] merges [from] into [repo]'s current active branch with the extra
    arguments [args]. *)

val git_resolve :
  remote:string -> ref:Git.Ref.t -> (Git.Ref.resolved, Rresult.R.msg) result
(** [git_resolve ~remote ~ref] runs git ls-remote to resolve the given ref to a commit hash *)

val git_branch :
  repo:Fpath.t ->
  ref:Git.Ref.t ->
  branch_name:string ->
  (unit, [> Rresult.R.msg ]) result

val git_submodule_add :
  repo:Fpath.t ->
  remote_name:string ->
  ref:Git.Ref.t ->
  branch:string ->
  target_path:string ->
  ?force:bool ->
  unit ->
  (unit, [> Rresult.R.msg ]) result
(** [git_submodule_add] will run [git submodule] for [remote_name] and initialise
   it into [target_path] for commit [ref] and on the remote [branch]. *)

val git_update_index :
  repo:Fpath.t ->
  ?add:bool ->
  cacheinfo:int * string * Fpath.t ->
  unit ->
  (unit, [> Rresult.R.msg ]) result
(** [git_update_index] will add the [cacheinfo] (a tuple of mode, hash and target path)
  to the index, and append it to the cache if [add] is [true]. *)

val git_remote_add :
  repo:Fpath.t ->
  remote_url:string ->
  remote_name:string ->
  (unit, [> Rresult.R.msg ]) result
(** Uses git remote add in repo **)

val git_remote_remove :
  repo:Fpath.t -> remote_name:string -> (unit, [> Rresult.R.msg ]) result
(** Uses git remote remove in repo **)

val git_fetch_to :
  repo:Fpath.t ->
  remote_name:string ->
  ref:string ->
  branch:string ->
  ?force:bool ->
  unit ->
  (unit, [> Rresult.R.msg ]) result
(** [git_fetch_to ~remote_name ~tag ~branch] Fetches tag from remote_name into a given branch **)

val git_init_bare : repo:Fpath.t -> (unit, [> Rresult.R.msg ]) result
(** [git_init path] Initialize Git as a bare repo in given path **)

val git_clone :
  branch:string ->
  remote:string ->
  output_dir:Fpath.t ->
  (unit, [> Rresult.R.msg ]) result
(** [git_clone ~branch ~remote ~output_dir] Git clone branch from remote in output_dir **)

val git_clone_or_pull :
  branch:string ->
  remote:string ->
  output_dir:Fpath.t ->
  (unit, [> Rresult.R.msg ]) result

val git_rename_branch_to :
  repo:Fpath.t -> branch:string -> (unit, [> Rresult.R.msg ]) result
(** [git_rename_branch_to ~branch] Sets repo's branch name to branch. **)

val git_remotes : repo:Fpath.t -> (string list, [> Rresult.R.msg ]) result
(** [git_remotes repo] List remotes of the git project located in repo. **)

val git_branch_exists : repo:Fpath.t -> branch:string -> bool
(** [git_branch_exists repo branch] Returns true if branch exists in repo. **)
