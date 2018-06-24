Require Import ZArith.
Require Import NArith.
Require Import Coq.Strings.String.
Require Import Ascii.

Require Import Packet.

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

Lemma parsed_op_correct_value: forall (op : opcode) (tail : string),
        parse_opcode (String zero (String
            (ascii_of_pos (opcode_value (op)))
            tail
        )) = Some(op, tail).
Proof.
    intros.
    destruct op.
    all: simpl; reflexivity.
Qed.

Definition parse_errcode (data : string) : option (errcode * string) :=
match data with
    | EmptyString => None
    | String zero (String op tail) =>
        match op with
            | "000" => Some(Undefined, tail)
            | "001" => Some(FileNotFound, tail)
            | "002" => Some(AccessViolation, tail)
            | "003" => Some(AllocError, tail)
            | "004" => Some(IllegalOp, tail)
            | "005" => Some(UnknownTID, tail)
            | "006" => Some(AlreadyExists, tail)
            | "007" => Some(NoSuchUser, tail)
            | _ => None
        end
    | _ => None
end.

Lemma parsed_err_correct_value: forall (err : errcode) (tail : string),
        parse_errcode (String zero (String
            (ascii_of_N (errcode_value (err)))
            tail
        )) = Some(err, tail).
Proof.
    intros.
    destruct err.
    all: simpl; reflexivity.
Qed.

(*
TFTP Formats
   Type   Op #     Format without header

          2 bytes    string   1 byte     string   1 byte
          -----------------------------------------------
   RRQ/  | 01/02 |  Filename  |   0  |    Mode    |   0  |
   WRQ    -----------------------------------------------
          2 bytes    2 bytes       n bytes
          ---------------------------------
   DATA  | 03    |   Block #  |    Data    |
          ---------------------------------
          2 bytes    2 bytes
          -------------------
   ACK   | 04    |   Block #  |
          --------------------
          2 bytes  2 bytes        string    1 byte
          ----------------------------------------
   ERROR | 05    |  ErrorCode |   ErrMsg   |   0  |
          ----------------------------------------
*)

(* ! Wykomentowane przez problemy z ekstrakcja *)
Definition parse_data (data: string) : option packet :=
    None.
    (* match data with
    | String c1 (String c2 tail) =>
        let num := (nat_of_ascii(c1) * 256) + nat_of_ascii(c2) in
            (* TODO: Handle mode: netascii replaces newline *)
            match (length tail) <= 512 with
            | True => Some(Data (N.of_nat num) tail)
            end
    | _ => None
    end. *)

Definition parse_error (data : string) : option packet :=
    None.
    (* match parse_errcode data with
    | Some(errcode, String c tail) =>
        let len := length (String c tail) in
        match get len (String c tail) with
        (* ErrMsg ends with 0 *)
        | Some("000") =>
            let errmsg := substring 0 (len-1) (String c tail) in
                Some(Error errcode errmsg)
        | _ => None
        end
    | _ => None
    end. *)

Definition parse_packet (data : string) : option packet :=
    None.
    (* match parse_opcode (data) with
    | Some(RRQ, tail) => None (* TODO *)
    | Some(WRQ, tail) => None (* TODO *)
    | Some(DATA, tail) => parse_data tail
    | Some(ACK, String c1 (String c2 EmptyString)) =>
        let num := (nat_of_ascii(c1) * 256) + nat_of_ascii(c2) in
            Some(Acknowledgment (N.of_nat num))
    | Some(ERROR, tail) => parse_error tail
    | _ => None
    end. *)

(* TODO *)
Definition string_of_packet (p : packet) : string := EmptyString.
