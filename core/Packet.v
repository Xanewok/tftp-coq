Require Import ZArith.

Local Open Scope positive_scope.

Unset Elimination Schemes. (* Don't generate _rect _ind principles *)
Inductive opcode: Type :=
    | RRQ    (* 1 = Read request *)
    | WRQ    (* 2 = Write request *)
    | DATA   (* 3 = Data *)
    | ACK    (* 4 = Acknowledgment *)
    | ERROR. (* 5 = Error *)

Definition opcode_value (op : opcode) : positive :=
    match op with
        | RRQ => 1
        | WRQ => 2
        | DATA => 3
        | ACK => 4
        | ERROR => 5
    end.

(* Unset Printing Notations. *)
Lemma opcode_limited: forall A, opcode_value(A) >= 1 /\ opcode_value(A) <= 5.
Proof.
    intros.
    unfold opcode_value.
    destruct A.
    all: easy.
Qed.
