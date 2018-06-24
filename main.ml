(*
* Parsować parametry wywołania
* Wchodzić w pętlę obsługi połączeń
* W pętli obsługi połączeń przyjmować połączenie
* Obsłużyć połączenie
* Powrócić do początku pętli obsługi połączeń
 *)
open Unix
open Printf
open Thread

open Server
open Packet

let maxlen = 1500 (* Ethernet MTU *)
let port = ref 8069

let set_port = function
| p when (p >= 1 && p <= 65535) -> port := p
| _ -> raise (Arg.Bad("Port outside of 1..65535 range"))

let speclist = [
    ("-p", Arg.Int (set_port), "Bind server to a given port number");
]

let rec accept_loop sock =
    let buffer = Bytes.create maxlen in
    let buffer, host, sockaddr =
    match recvfrom sock buffer 0 maxlen [] with
        | len, (ADDR_INET (addr, port) as sockaddr) ->
            String.sub buffer 0 len,
            (gethostbyaddr addr).h_name,
            sockaddr
        | _ -> assert false
    in
    printf "Client %s said ``%s''\n%!" host buffer;

    accept_loop sock

let () =
    Arg.parse speclist print_endline "Simple Coq-certified TFTP Server";
    print_endline (string_of_int (!port));

    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    let sock = socket PF_INET SOCK_DGRAM 0 in
    setsockopt sock SO_REUSEADDR true;
    bind sock (ADDR_INET (inet_addr_any, !port));
    printf "Entering UDP packet loop...\n%!";
    accept_loop sock
