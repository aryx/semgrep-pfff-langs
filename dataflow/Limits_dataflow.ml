(* Dataflow analysis limits.
 *
 * These constants were originally in Limits_semgrep (in osemgrep/src/configuring/).
 * They are here so that semgrep-pfff-langs does not depend on semgrep.
 *)

(* Minimum number of fixpoint iterations to run before checking for timeouts. *)
let fixpoint_MIN_ITERS = 100

(* Timeout in seconds for svalue propagation fixpoint. *)
let svalue_prop_FIXPOINT_TIMEOUT = 0.15

(* Bounds the number of times we follow an id_svalue during a cycle check. *)
let svalue_prop_MAX_VISIT_SYM_IN_CYCLE_CHECK = 1000
