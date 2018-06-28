(* Sekcja 2,4,6,7 w RFC *)
(* Zamodelowanie pakietów z sekcji 5 *)
(* Funkcje zależeć będą od stanu wewnętrznego serwera, czyli:
 * stan protokołu (automat stanowy odczytac z sekcji 2,4,6,7)
 * opcjonalnie przyslany od klienta pakiet
 * informacje o pliku lokalnym, ktory jest przesylany w komunikacji
 *)
(* Funkcje będą produkowały nowy stan wewnetrzny oraz opcjonalnie pakiet,
jaki ma zostac wyslany do klienta. Pakiety maja byc reprezentowane jako
wektor osmiobitowych znakow. *)

Require Import ZArith.
Require Import String.

Require Import Packet.

Unset Elimination Schemes.
Inductive mode : Set :=
    | Read  : mode
    | Write : mode.

Record init_state : Set := mkState {
    transfer_mode : mode;
    filename : string;
    port: positive;
    last_packet : option packet;
    timeout_count : N;
}.

(* None is standby state - waiting for read/write requests *)
Definition state : Type := option init_state.

(* Connection is terminated by the server after this many timeouts *)
Definition max_timeout_count : positive := 5.

(* Event on which server operates *)
Inductive event : Set :=
    | Timeout : positive -> event (* # of timeouts *)
    | Packet  : packet -> event.

(* TODO *)
(* Action (filename offset byte_count [buf]) *)
Inductive io_action : Set :=
    | ReadAction  : string -> N -> N -> io_action
    | WriteAction : string -> N -> N -> string -> io_action.

Inductive io_result : Set :=
    | DidRead : string -> io_result
    | DidWrite : io_result
    | IOFail.

Definition incr_timeout (st : init_state) : init_state :=
    mkState
    (transfer_mode st) (filename st) (port st) (last_packet st) ((timeout_count st) + 1).

(* TODO *)
Local Open Scope string_scope.
Local Open Scope positive_scope.
Definition request_io (st : state) (ev : event) : option io_action :=
    match st with
    | None =>
        match ev with
        | Packet (ReadReq file _) => Some(ReadAction file 0 512)
        | _ => None
        end
    | Some st =>
        match ev with
        | Packet (Acknowledgment num) =>
            match transfer_mode st with
            | Read => Some(ReadAction (filename st) (num + 1) 512)
            | Write => None (* Invalid *)
            end
        | Packet (Data num buf) =>
            match transfer_mode st with
            | Write => Some(WriteAction (filename st) (num + 1) 512 buf)
            | Read => None (* Invalid *)
            end
        | _ => None
        end
    end.

Definition handle_event (st : state) (ev : event) (port : positive) (act : option io_result)
(* new state, packet, requested io_action, should terminate *)
: (state * option packet * bool) :=
    match st with
    | None =>
        match ev with
        | Timeout count => (None, Some (Error Undefined ""), false) (* Can't timeout on uninitialized *)
        | Packet (ReadReq file mode) =>
            ((Some (mkState Read file port None 0)), Some (Acknowledgment 0), false)
        | Packet (WriteReq file mode) =>
            ((Some (mkState Write file port None 0)), Some (Acknowledgment 0), false)
        | _ => (None, Some (Error IllegalOp ""), true) (* Only RRQ/WRQ can be initial messages *)
        end
    | Some st =>
        match ev with
        | Timeout count =>
            match max_timeout_count <=? count with
            | true => (None, Some (Error Undefined "Timed out"), true)
            | false => (Some (incr_timeout st), None, false)
            end
        | _ => (None, Some (Error IllegalOp ""), true) (* TODO *)
        end
    end.

From Coq Require Extraction.
Extract Inlined Constant Pos.leb => "(<=)".