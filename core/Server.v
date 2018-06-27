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
    | ReadAction  : string -> io_action
    | WriteAction : string -> io_action.

(* TODO *)
Definition handle_event (state : state) (ev : event)
: option (init_state * (option packet)) :=
    Some ((mkState Read 1 None), None).
