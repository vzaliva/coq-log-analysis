
Require Import MathClasses.interfaces.canonical_names.

Parameter foo: Type.
Instance fequiv: Equiv foo. Admitted.
Instance frefl: Reflexive fequiv. Admitted.
(* Instance ftrans: Transitive fequiv. Admitted. *)

Parameter bar: (foo -> foo) -> foo -> foo.

(* Instance bar_proper: Proper (((=) ==> (=)) ==> (=) ==> (=)) (bar). *)
Instance bar_proper: Proper ((=) ==> (=) ==> (=)) (bar). Admitted.

Lemma Test
      (dot : foo -> foo)
      {dot_proper: Proper ((=) ==> (=)) (dot)}
      (x y: foo)
      (H: x=y):
  bar dot x = bar dot y.
Proof.
  Set Typeclasses Debug.
  Set Typeclasses Debug Verbosity 2.
  Set Printing All.
  Fail Redirect "test.txt"
           setoid_rewrite H.
Admitted.
