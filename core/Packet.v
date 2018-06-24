Require Import ZArith.
Require Import Coq.Strings.String.
Require Import Ascii.

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

Local Open Scope char_scope.
Definition parse_opcode (data : string) : option (opcode * string) :=
    match data with
        | EmptyString => None
        | String zero (String op tail) =>
            match op with
                | "001" => Some(RRQ, tail)
                | "002" => Some(WRQ, tail)
                | "003" => Some(DATA, tail)
                | "004" => Some(ACK, tail)
                | "005" => Some(ERROR, tail)
                | _ => None
            end
        | _ => None
    end.

Lemma parsed_op_correct_value:
    forall (op : opcode) (tail : string),
        parse_opcode (String zero (String
            (ascii_of_pos (opcode_value (op)))
            tail
        )) = Some(op, tail).
Proof.
    intros.
    destruct op.
    all: simpl; reflexivity.
Qed.
