(*
* Parsować parametry wywołania
* Wchodzić w pętlę obsługi połączeń
* W pętli obsługi połączeń przyjmować połączenie
* Obsłużyć połączenie
* Powrócić do początku pętli obsługi połączeń
 *)
open Unix
open Thread
open Printf

open Server
open Packet
open Serialize

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

let timeout = 5.0 (* TFTP message timeout *)
let maxlen = 1500 (* Ethernet MTU *)
let port = ref 69

let set_port = function
| p when (p >= 0 && p <= 65535) -> port := p
| _ -> raise (Arg.Bad("Port outside of 0..65535 range"))

let speclist = [
    ("-p", Arg.Int (set_port), "Bind server to a given port number");
]

let debug_mode_to_string mode =
    match mode with
    | Netascii -> "netascii"
    | Octet -> "octet"

let debug_packet_to_debug packet =
    match packet with
        | ReadReq (clist, mode) -> sprintf "RRQ: %s %s" (implode clist) (debug_mode_to_string mode)
        | WriteReq (clist, mode) -> sprintf "WRQ: %s %s" (implode clist) (debug_mode_to_string mode)
        | Data (num, clist) -> sprintf "DATA: %d %s" num (implode clist)
        | Acknowledgment (num) -> sprintf "ACK: %d" num
        | Error (errcode, clist) -> sprintf "ERROR: %d %s" (errcode_value errcode) (implode clist)

(* val open_socket : int -> Unix.file_descr option *)
let open_socket timeout port =
    try
        let sock = socket PF_INET SOCK_DGRAM 0 in
        match timeout with
            | Some(timeout) -> setsockopt_float sock SO_RCVTIMEO timeout;
            | None -> ();
        ;
        setsockopt sock SO_REUSEADDR true;
        bind sock (ADDR_INET (inet_addr_any, port));
        Some sock
    with Unix_error (_, _, _) -> None

let receive_packet sock =
    let buffer = Bytes.create maxlen in
    match
        try Some (Some (recvfrom sock buffer 0 maxlen []))
        with
            | Unix_error (EAGAIN, _, _) -> Some(None)
            | _ -> None
    with
        | Some (Some(len, (ADDR_INET (addr, port) as sockaddr))) ->
            (* buffer, host, port, sockaddr *)
            Some (Some (
                String.sub buffer 0 len,
                (gethostbyaddr addr).h_name,
                port,
                sockaddr
            ))
        | Some (Some(_, ADDR_UNIX _)) -> assert false
        | Some None -> Some None
        | None -> None

let rec conn_loop state sock =
    (match receive_packet sock with
        | None -> printf "[error] Error receiving packet from TID: %d\n%!" (Server.port !state)
        | Some None -> printf "[debug] Packet timed out in TID: %d\n%!" (Server.port !state)
        | Some Some(buffer, host, port, sockaddr) ->
            match parse_packet(explode(buffer)) with
                | None -> printf "[error] Couldn't parse: %s\n%!" buffer;
                | Some(packet) ->
                    printf "[debug] Received packet: %s\n%!" (debug_packet_to_debug packet);
                    (* TODO: Handle <512 data packets (connection termination) *)
                    match handle_event None (Packet packet) with
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
    match open_socket (Some timeout) 0 with
        | None -> printf "[error] Couldn't open a socket for a new connection%!";
        | Some(sock) ->
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
            printf "[debug] Received packet: %s\n%!" (debug_packet_to_debug packet);
            (* TODO: Check if the packet is valid RRQ/WRQ *)
            match handle_event None (Packet packet) with
            | None -> ();
            | Some(state, response) ->
                let handle = Thread.create conn_entry
                    (sockaddr, state, packet, response) in
                        printf "[debug] Starting connection thread id:%d\n%!" (Thread.id handle);
    );

    recv_loop sock

let () =
    Arg.parse speclist print_endline "Simple Coq-certified TFTP Server";

    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    match open_socket None !port with
    | None -> printf "[error] Couldn't start a server on port %d\n%!" !port;
    | Some(sock) ->
        printf "[debug] Entering UDP packet loop (port: %d)...\n%!" !port;
        recv_loop sock
