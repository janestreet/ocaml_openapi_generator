open! Core
open! Async

let main_command =
  Command.group
    ~summary:"inspect and transform OpenAPI specifications"
    [ "inspect", Inspect_command.command; "generate", Generate_command.command ]
;;

let () = Command_unix.run main_command
