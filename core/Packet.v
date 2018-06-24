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
        | RRQ   => 1
        | WRQ   => 2
        | DATA  => 3
        | ACK   => 4
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

Inductive mode: Type :=
    | Netascii
    | Octet.

Inductive errcode: Type :=
    | Undefined       (* Not defined, see error message (if any). *)
    | FileNotFound    (* File not found. *)
    | AccessViolation (* Access violation. *)
    | AllocError      (* Disk full or allocation exceeded. *)
    | IllegalOp       (* Illegal TFTP operation. *)
    | UnknownTID      (* Unknown transfer ID. *)
    | AlreadyExists   (* File already exists. *)
    | NoSuchUser.     (* No such user. *)

Definition errcode_value (err : errcode) : N :=
    match err with
        | Undefined       => 0
        | FileNotFound    => 1
        | AccessViolation => 2
        | AllocError      => 3
        | IllegalOp       => 4
        | UnknownTID      => 5
        | AlreadyExists   => 6
        | NoSuchUser      => 7
    end.

Inductive packet : Set :=
    | ReadReq        : string -> mode -> packet
    | WriteReq       : string -> mode -> packet
    | Data           : N -> string -> packet
    | Acknowledgment : N -> packet
    | Error          : errcode -> string -> packet.

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


(* TODO *)
Definition parse_packet (msg : string) : option packet :=
    None.

(* TODO *)
Definition string_of_packet (p : packet) : string := EmptyString.