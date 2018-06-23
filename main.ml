(*
* Parsować parametry wywołania
* Wchodzić w pętlę obsługi połączeń
* W pętli obsługi połączeń przyjmować połączenie
* Obsłużyć połączenie
* Powrócić do początku pętli obsługi połączeń
 *)
open Server
open Packet

let a =
    RRQ

let () =
    print_string (string_of_int (opcode_value RRQ))
    (* print_string "Hello world!\n";; *)
