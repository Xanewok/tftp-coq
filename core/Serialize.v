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

Require Import ExtrOcamlNatInt.
Require Import ExtrOcamlZInt.

Require Import ZArith.
Require Import Coq.Strings.String.
Require Import Ascii.

Require Import Packet.

Local Open Scope char_scope.
Local Open Scope N_scope.

Fixpoint consume_cstring (input : string) : option (string * string) :=
    match input with
    | String c tail =>
        match N_of_ascii(c) with
        | 0 %N => Some (EmptyString, tail)
        | _ =>
            match consume_cstring(tail) with
            | Some (cstring, tail) => Some ((String c cstring), tail)
            | _ => None
            end
        end
    | _ => None
    end.

Definition parse_opcode (data : string) : option (opcode * string) :=
    match data with
    | EmptyString => None
    | String zero (String op tail) =>
        match N_of_ascii(op) with
        | 1 %N => Some(RRQ, tail)
        | 2 %N => Some(WRQ, tail)
        | 3 %N => Some(DATA, tail)
        | 4 %N => Some(ACK, tail)
        | 5 %N => Some(ERROR, tail)
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
        match N_of_ascii(op) with
        | 0 %N => Some(Undefined, tail)
        | 1 %N => Some(FileNotFound, tail)
        | 2 %N => Some(AccessViolation, tail)
        | 3 %N => Some(AllocError, tail)
        | 4 %N => Some(IllegalOp, tail)
        | 5 %N => Some(UnknownTID, tail)
        | 6 %N => Some(AlreadyExists, tail)
        | 7 %N => Some(NoSuchUser, tail)
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

Definition parse_data (data: string) : option packet :=
    match data with
    | String c1 (String c2 tail) =>
        let num := (N_of_ascii(c1) * 256) + N_of_ascii(c2) in
            (* TODO: Handle mode: netascii replaces newline *)
            match ((length tail) <= 512)%nat with
            | True => Some(Data num tail)
            end
    | _ => None
    end.

Definition parse_error (data : string) : option packet :=
    match parse_errcode data with
    | Some(errcode, tail) =>
        match consume_cstring tail with
        | Some(msg, tail) => Some(Error errcode msg)
        | None => None
        end
    | None => None
    end.

Local Open Scope string_scope.
Definition parse_mode (data : string) : option mode :=
    match data with
    | "netascii" => Some(Netascii)
    | "octet" => Some(Octet)
    | _ => None
    end.
Local Close Scope string_scope.

Definition parse_request_body (data : string) : option (string * mode) :=
    match consume_cstring data with
    | Some(filename, tail) =>
        match consume_cstring tail with
        | Some(mode, EmptyString) =>
            match parse_mode mode with
            | Some(mode) => Some(filename, mode)
            | None => None
            end
        | _ => None
        end
    | _ => None
    end.

Definition parse_packet (data : string) : option packet :=
    match parse_opcode (data) with
    | Some(RRQ, tail) =>
        match parse_request_body tail with
        | Some(filename, mode) => Some(ReadReq filename mode)
        | None => None
        end
    | Some(WRQ, tail) =>
        match parse_request_body tail with
        | Some(filename, mode) => Some(WriteReq filename mode)
        | None => None
        end
    | Some(DATA, tail) => parse_data tail
    | Some(ACK, String c1 (String c2 EmptyString)) =>
        let num := (N_of_ascii(c1) * 256) + N_of_ascii(c2) in
            Some(Acknowledgment num)
    | Some(ERROR, tail) => parse_error tail
    | _ => None
    end.

Theorem data_packet_payload : forall c1 c2 tail num data,
    parse_packet(
        String "000" (
        String (ascii_of_pos (opcode_value DATA)) (
        String c1 (
        String c2 tail)))
    ) = Some(Data num data) -> data = tail /\ num = N_of_ascii(c1) * 256 + N_of_ascii(c2).
Proof.
    intros.
    unfold parse_packet in H.
    simpl in H.
    injection H as H1.
    auto.
Qed.

From Coq Require Extraction.
Extract Inlined Constant Init.Nat.sub => "(-)".
Extract Inlined Constant N_of_ascii => "Char.code".
Extract Inlined Constant N.mul => "( * )".
Extract Inlined Constant N.add => "(+)".
Extract Inlined Constant String.length => "List.length".
(* Extract Inlined Constant String.substring =>
    "(let rec sublist b e l =
    match l with
      |  [] -> []
      | h :: t ->
         let tail = if e=0 then [] else sublist (b-1) (e-1) t in
         if b>0 then tail else h :: tail
    in sublist)". *)
Extract Inlined Constant String.get => (* TODO: Coq is 1-based, Ocaml is 0-based *)
    "(fun x y -> try Some (List.nth y x) with Failure f -> None)".