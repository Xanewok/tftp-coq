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
Inductive io_action : Set :=
    | ReadAction  : string -> N -> io_action
    | WriteAction : string -> N -> string -> io_action.

Definition incr_timeout (st : init_state) : init_state :=
    mkState
    (transfer_mode st) (port st) (last_packet st) ((timeout_count st) + 1).

(* TODO *)
Local Open Scope string_scope.
Local Open Scope positive_scope.
Definition handle_event (st : state) (ev : event) (port : positive)
(* new state, packet, requested io_action, should terminate *)
: (state * option packet * option io_action * bool) :=
    match st with
    | None =>
        match ev with
        | Timeout count => (None, Some (Error Undefined ""), None, false) (* Can't timeout on uninitialized *)
        | Packet (ReadReq file mode) =>
            ((Some (mkState Read port None 0)), Some (Acknowledgment 0), None, false)
        | Packet (WriteReq file mode) =>
            ((Some (mkState Write port None 0)), Some (Acknowledgment 0), None, false)
        | _ => (None, Some (Error IllegalOp ""), None, true) (* Only RRQ/WRQ can be initial messages *)
        end
    | Some st =>
        match ev with
        | Timeout count =>
            match max_timeout_count <=? count with
            | true => (None, Some (Error Undefined "Timed out"), None, true)
            | false => (Some (incr_timeout st), None, None, false)
            end
        | _ => (None, Some (Error IllegalOp ""), None, true) (* TODO *)
        end
    end.

From Coq Require Extraction.
Extract Inlined Constant Pos.leb => "(<=)".