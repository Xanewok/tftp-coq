Require Import ZArith.
Require Import Coq.Strings.String.
Require Import Ascii.

Local Open Scope positive_scope.

Unset Elimination Schemes. (* Don't generate _rect _ind principles *)
Inductive opcode : Set :=
    | RRQ   : opcode   (* 1 = Read request *)
    | WRQ   : opcode   (* 2 = Write request *)
    | DATA  : opcode   (* 3 = Data *)
    | ACK   : opcode   (* 4 = Acknowledgment *)
    | ERROR : opcode.  (* 5 = Error *)

Definition opcode_value (op : opcode) : positive :=
    match op with
        | RRQ   => 1
        | WRQ   => 2
        | DATA  => 3
        | ACK   => 4
        | ERROR => 5
    end.

Lemma opcode_limited : forall A, opcode_value(A) <= 5.
Proof.
    intros.
    destruct A.
    all: easy.
Qed.

Inductive mode : Set :=
    | Netascii : mode
    | Octet    : mode.

Inductive errcode : Set :=
    | Undefined       : errcode  (* Not defined, see error message (if any). *)
    | FileNotFound    : errcode  (* File not found. *)
    | AccessViolation : errcode  (* Access violation. *)
    | AllocError      : errcode  (* Disk full or allocation exceeded. *)
    | IllegalOp       : errcode  (* Illegal TFTP operation. *)
    | UnknownTID      : errcode  (* Unknown transfer ID. *)
    | AlreadyExists   : errcode  (* File already exists. *)
    | NoSuchUser      : errcode. (* No such user. *)

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

Local Open Scope N_scope.
Lemma errcode_limited: forall A : errcode, errcode_value(A) <= 7.
Proof.
    intros.
    destruct A.
    all: easy.
Qed.

Inductive packet : Set :=
    | ReadReq        : string -> mode -> packet
    | WriteReq       : string -> mode -> packet
    | Data           : N -> string -> packet
    | Acknowledgment : N -> packet
    | Error          : errcode -> string -> packet.
