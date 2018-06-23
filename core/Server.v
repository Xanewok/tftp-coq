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

Require Import Packet.

Definition lul(m: opcode) := m.

