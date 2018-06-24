Require Import ExtrOcamlZInt.
Require Import ExtrOcamlString.
Require Import ExtrOcamlBasic.

(* Don't extract _rec(t) definitions *)
Unset Elimination Schemes.

Require Import Packet.
Require Import Server.

Extraction Library Packet.
Extraction Library Server.