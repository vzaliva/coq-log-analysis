

Require Import Relations Setoid Morphisms.

Parameter Foo: Type.
Parameter R: relation Foo.
Parameter bar: Foo -> Foo -> Foo.

Instance R_reflexive: Reflexive R. Admitted.
Instance R_trasitive: Transitive R. Admitted.
Instance bar_proper: Proper ((R) ==> (R) ==> (R)) (bar). Admitted.

Example Test (a b c: Foo):
  R a b -> R (bar c a) (bar c b).
Proof.
  intros H.
  Set Typeclasses Debug Verbosity 2.
  Set Printing All.
  Redirect "test.txt"
           setoid_rewrite H.
Admitted.

(*

(* With math classes. *)

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
*)