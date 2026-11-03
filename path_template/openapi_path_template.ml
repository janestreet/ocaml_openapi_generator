module Path_component = struct
  type t =
    | Constant of string
    | Parameter of string
end

type t = Path_component.t list

let parse_component str : Path_component.t =
  let len = String.length str in
  if len >= 2 && Char.equal str.[0] '{' && Char.equal str.[len - 1] '}'
  then Parameter (String.sub str 1 (len - 2))
  else Constant str
;;

let of_template_string template =
  match String.split_on_char '/' template with
  | [] -> None
  | _ :: components -> Some (List.map parse_component components)
;;
