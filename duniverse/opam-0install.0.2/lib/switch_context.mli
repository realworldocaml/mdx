include S.CONTEXT

val create :
  ?test:OpamPackage.Name.Set.t ->
  constraints:OpamFormula.version_constraint OpamTypes.name_map ->
  OpamStateTypes.unlocked OpamStateTypes.switch_state ->
  t
(** [create ~constraints switch] is a solver that gets candidates from [switch], filtering them
    using [constraints].
    @param test Packages for which we should include "with-test" dependencies. *)
