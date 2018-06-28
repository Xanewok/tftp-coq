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

let debug_packet_to_string packet =
    match packet with
        | ReadReq (clist, mode) -> sprintf "RRQ: %s %s" (implode clist) (debug_mode_to_string mode)
        | WriteReq (clist, mode) -> sprintf "WRQ: %s %s" (implode clist) (debug_mode_to_string mode)
        | Data (num, clist) -> sprintf "DATA: %d %s" num (implode clist)
        | Acknowledgment (num) -> sprintf "ACK: %d" num
        | Error (errcode, clist) -> sprintf "ERROR: %d %s" (errcode_value errcode) (implode clist)

let bytes_of_opcode op = [Char.chr 0; Char.chr (opcode_value op)]
let bytes_of_ercode er = [Char.chr 0; Char.chr (errcode_value er)]
let bytes_of_16bits vl = [Char.chr (vl lsr 8); Char.chr (vl mod 256)]

let bytes_of_packet packet =
    match packet with
        | Data (num, msg) -> (bytes_of_opcode DATA) @ msg
        | Acknowledgment num -> (bytes_of_opcode ACK) @ (bytes_of_16bits num)
        | Error (code, msg) -> (bytes_of_opcode ERROR) @ bytes_of_ercode code @ msg @ [Char.chr 0]
        | _ -> []

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
    let myport = (!state).port in
    (match receive_packet sock with
        | None -> printf "[error] [%5d] Error receiving packet\n%!" myport
        | Some None -> printf "[debug] [%5d] Packet timed out\n%!" myport
        | Some Some(buffer, host, port, sockaddr) ->
            match parse_packet(explode(buffer)) with
                | None -> printf "[error] [%5d] Couldn't parse: %s\n%!" myport buffer;
                | Some(packet) ->
                    printf "[debug] [%5d] Received packet: %s\n%!" myport (debug_packet_to_string packet);
                    (* TODO: Handle <512 data packets (connection termination) *)
                    match handle_event (Some !state) (Packet packet) port with
                    | (((_, Some resp), _), true) -> (* Fatal - respond and forget *)
                        printf "[debug] [%5d] Protocol error - responding with %s\n%!" myport (debug_packet_to_string resp);
                        let resp = bytes_of_packet resp in
                        ignore (sendto sock (implode resp) 0 (List.length resp) [] sockaddr);
                        Thread.exit ();
                    | (((_, Some resp), None), false) when (Packet.is_error resp) -> (* Non-fatal error *)
                        let resp = bytes_of_packet resp in
                        ignore (sendto sock (implode resp) 0 (List.length resp) [] sockaddr);
                    | (((Some state', Some resp), None), false) ->
                        let resp = bytes_of_packet resp in
                        ignore (sendto sock (implode resp) 0 (List.length resp) [] sockaddr);
                        state := state';
                    | _ -> (); (* TODO *)
    );

    conn_loop state sock

(* Serves as an entry-point for incoming connections - port number is generated
 * that is used for further packets regarding this connection *)
let conn_entry (sockaddr, init_state) =
    match open_socket (Some timeout) 0 with
        | None -> printf "[error] Couldn't open a socket for a new connection%!";
        | Some(sock) ->
            let state = ref init_state in
            printf "[debug] [%5d] Started new connection thread\n%!" (!state).port;
            conn_loop state sock

let rec recv_loop sock =
    match receive_packet sock with
    | None -> printf "[error] Error receiving packet\n%!"
    | Some None -> assert false (* No timeout is expected in main loop *)
    | Some Some(buffer, host, port, sockaddr) ->
        (printf "[debug] Cient %s:%d said ``%s''\n%!" host port buffer;

        match parse_packet(explode(buffer)) with
            | None -> printf "[error] Couldn't parse: %s\n%!" buffer;
            | Some(packet) ->
                printf "[debug] Received packet: %s\n%!" (debug_packet_to_string packet);
                (* TODO *)
                match handle_event None (Packet packet) port with
                | (((_, Some resp), _), true) -> (* Error - respond and forget *)
                    printf "[debug] Protocol error - responding with %s\n%!" (debug_packet_to_string resp);
                    let resp = bytes_of_packet resp in
                    ignore (sendto sock (implode resp) 0 (List.length resp) [] sockaddr);
                | (((Some state', Some resp), None), false) -> (* No action should be done when initializing *)
                    let resp = bytes_of_packet resp in
                    ignore (sendto sock (implode resp) 0 (List.length resp) [] sockaddr);
                    ignore (Thread.create conn_entry (sockaddr, state'))
                | _ -> assert false
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
