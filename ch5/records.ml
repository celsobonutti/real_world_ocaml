open Core

type service_info =
  { service_name  : string;
    port          : int;
    protocol      : string;
    comment       : string option;
  }

let service_info_of_string line =
  (* first, split off any comment *)
  let (line, comment) =
    match String.rsplit2 line ~on:'#' with
    | None -> (line, None)
    | Some (ordinary, comment) -> (ordinary, Some comment)
  in
  let matches =
    Re.exec (Re.Posix.compile_pat "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)") line
  in
  let service_name = Re.Group.get matches 1 in
  let port = Int.of_string (Re.Group.get matches 2) in
  let protocol = Re.Group.get matches 3 in
{ service_name;
  port;
  protocol;
  comment
}

type 'a with_line_number = { item: 'a; line_num: int }

let parse_lines parse file_contents =
  let lines = String.split ~on:'\n' file_contents in
  List.mapi lines ~f:(fun line_num line ->
      { item = parse line;
        line_num = line_num + 1;})

let _: service_info with_line_number list  = parse_lines service_info_of_string
    "rtmp           1/ddp       # Routine Table Maintenance Protocol
     tcpmux         1/udp       # TCP Port Service Multiplexer
     tcpmux         1/tcp       # TCP Port Service Multiplexer"

let service_info_to_string { service_name = name; port = port; protocol = prot; _ } =
  sprintf "%s %i/%s" name port prot

let create_service_info ~service_name ~port ~protocol ~comment =
  { service_name; port; protocol; comment }

module Log_entry = struct
  type t =
    { session_id: string;
      time: Time_ns.t;
      important: bool;
      message: string;
    }
end

module Heartbeat = struct
  type t =
    { session_id: string;
      time: Time_ns.t;
      status_message: string;
    }
end

module Logon = struct
  type t =
    { session_id: string;
      time: Time_ns.t;
      user: string;
      credentials: string;
    }
    [@@deriving fields]
end

let create_log_entry ~session_id ~important message =
  { Log_entry.time = Time_ns.now ();
    Log_entry.session_id;
    Log_entry.important;
    Log_entry.message;
  }

type client_info =
  { addr: Unix.Inet_addr.t;
    port: int;
    user: string;
    credentials: string;
    mutable last_heartbeat_time: Time_ns.t;
    mutable last_heartbeat_status: string;
  }

let register_heartbeat t hb =
  { t with last_heartbeat_time = hb.Heartbeat.time;
           last_heartbeat_status = hb.Heartbeat.status_message;
  }

let register_heartbeat_mut t hb =
  t.last_heartbeat_time <- hb.Heartbeat.time;
  t.last_heartbeat_status <- hb.Heartbeat.status_message

let get_users logons =
  List.map logons ~f:Logon.user
  |> List.dedup_and_sort ~compare:String.compare

let show_field field to_string record =
  let name = Field.name field in
  let field_string = to_string (Field.get field record) in
  name ^ ": " ^ field_string

let logon = { Logon.
              session_id = "26685";
              time = Time_ns.of_string "2017-07-21 10:11:45 EST";
              user = "yminsky";
              credentials = "Xy2d9W";
            }

let print_logon logon =
  let print to_string field =
    printf "%s\n" (show_field field to_string logon)
  in
  Logon.Fields.iter
    ~session_id:(print Fn.id)
    ~time:(print Time_ns.to_string)
    ~user:(print Fn.id)
    ~credentials:(print Fn.id)
