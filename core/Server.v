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

Require Import Packet.

Unset Elimination Schemes.
Inductive mode : Set :=
    | Read  : mode
    | Write : mode.

Record init_server_state : Type := mkState {
    transfer_mode : mode;
    port: positive;
    last_packet : option packet;
}.

(* None is standby state - waiting for read/write requests *)
Definition server_state : Type := option init_server_state.

(* TODO *)
Definition handle_msg (state : server_state) (a : option packet)
: option (init_server_state * (option packet)) :=
    Some ((mkState Read 1 None), None).
