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

(* string <-> char list conversion functions *)
let explode s =
  let rec f acc = function
    | -1 -> acc
    | k -> f (s.[k] :: acc) (k - 1)
  in f [] (String.length s - 1)

let implode l =
  let res = Bytes.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> Bytes.set res i c; imp (i + 1) l in
  imp 0 l;;

module ThreadSet = Set.Make(struct type t = Thread.t let compare = compare end)

let connections = ref ThreadSet.empty
let maxlen = 1500 (* Ethernet MTU *)
let port = ref 8069

let set_port = function
| p when (p >= 1 && p <= 65535) -> port := p
| _ -> raise (Arg.Bad("Port outside of 1..65535 range"))

let speclist = [
    ("-p", Arg.Int (set_port), "Bind server to a given port number");
]

let rec conn_loop state sock =
    let buffer = Bytes.create maxlen in
    let buffer, host, port, sockaddr =
    match recvfrom sock buffer 0 maxlen [] with
        | len, (ADDR_INET (addr, port) as sockaddr) ->
            String.sub buffer 0 len,
            (gethostbyaddr addr).h_name,
            port,
            sockaddr
        | _ -> assert false
    in
    (match parse_packet(explode(buffer)) with
        | None -> printf "[error] Couldn't parse: %s\n%!" buffer;
        | Some(packet) ->
            (* TODO: Handle <512 data packets (connection termination) *)
            match handle_msg None (Some(packet)) with
            | None -> ();
            | Some(state', response) ->
                state := state';
                match response with
                | None -> ();
                | Some(response) -> let data = string_of_packet(response) in
                    ignore
                    (sendto sock (implode data) (List.length data) 0 [] sockaddr);
            ();
    );

    conn_loop state sock

(* Serves as an entry-point for incoming connections - port number is generated
 * that is used for further packets regarding this connection *)
let conn_entry (sockaddr, init_state, msg, response) =
    let sock = socket PF_INET SOCK_DGRAM 0 in
    setsockopt sock SO_REUSEADDR true;
    bind sock (ADDR_INET (inet_addr_any, 0));
    let port =
        match getsockname(sock) with
            | ADDR_INET (addr, port) -> port
            | ADDR_UNIX _ -> assert false
    in
    printf "[debug] Allocated port %d for new connection\n%!" port;
    let state = ref init_state in
    conn_loop state sock

let rec recv_loop sock =
    let buffer = Bytes.create maxlen in
    let buffer, host, port, sockaddr =
    match recvfrom sock buffer 0 maxlen [] with
        | len, (ADDR_INET (addr, port) as sockaddr) ->
            String.sub buffer 0 len,
            (gethostbyaddr addr).h_name,
            port,
            sockaddr
        | _ -> assert false
    in
    printf "[debug] Cient %s:%d said ``%s''\n%!" host port buffer;

    (match parse_packet(explode(buffer)) with
        | None -> printf "[error] Couldn't parse: %s\n%!" buffer;
        | Some(packet) ->
            (* TODO: Check if the packet is valid RRQ/WRQ *)
            match handle_msg None (Some(packet)) with
            | None -> ();
            | Some(state, response) ->
                let handle = Thread.create conn_entry
                    (sockaddr, state, packet, response) in
                        printf "[debug] Starting connection thread id:%d\n%!" (Thread.id handle);
                        connections := ThreadSet.add handle !connections;
    );

    recv_loop sock

let () =
    Arg.parse speclist print_endline "Simple Coq-certified TFTP Server";

    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    let sock = socket PF_INET SOCK_DGRAM 0 in
    setsockopt sock SO_REUSEADDR true;
    bind sock (ADDR_INET (inet_addr_any, !port));
    printf "[debug] Entering UDP packet loop (port: %d)...\n%!" !port;
    recv_loop sock
