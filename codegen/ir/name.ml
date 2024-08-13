open! Core

module T = struct
  type t =
    { sanitized : string
    ; raw : string
    ; namespace : string option
    }
  [@@deriving sexp, compare]
end

include T

let drop_consecutive_underscores str =
  String.to_list str
  |> List.mapi ~f:(fun ind c -> ind, c)
  |> List.filter_map ~f:(fun (ind, ch) ->
    if ind > 0 && Char.equal ch '_' && Char.equal str.[ind - 1] '_' then None else Some ch)
  |> String.of_list
;;

let to_snake_case str =
  let is_all_caps =
    String.for_all str ~f:(fun c -> if Char.is_alpha c then Char.is_uppercase c else true)
  in
  if is_all_caps
  then String.lowercase str
  else
    String.to_list str
    |> List.mapi ~f:(fun index c ->
      if Char.is_uppercase c && Int.O.(index > 0)
      then (
        let c = Char.lowercase c in
        let preceding_character_is_not_uppercase =
          let preceding = index - 1 in
          not (Char.is_uppercase str.[preceding])
        and following_character_exists_and_is_not_uppercase =
          let following = index + 1 in
          Int.O.(following < String.length str) && not (Char.is_uppercase str.[following])
        in
        (* Series of uppercase letters should be interpreted as acronyms, and therefore
           grouped in a single word. *)
        if preceding_character_is_not_uppercase
           || following_character_exists_and_is_not_uppercase
        then [ '_'; c ]
        else [ c ])
      else [ c ])
    |> List.concat
    |> String.of_char_list
    |> String.lowercase
    |> drop_consecutive_underscores
;;

let%expect_test "Test to_snake_case" =
  print_endline (to_snake_case "SomeCamelCaseString");
  [%expect {| some_camel_case_string |}];
  print_endline (to_snake_case "some_snake_case_string");
  [%expect {| some_snake_case_string |}];
  print_endline (to_snake_case "someCamelCaseString");
  [%expect {| some_camel_case_string |}];
  print_endline (to_snake_case "SomeCAMELCaseStringWithAcronym");
  [%expect {| some_camel_case_string_with_acronym |}];
  print_endline (to_snake_case "SOME_UPPER_CASE_STRING");
  [%expect {| some_upper_case_string |}];
  print_endline (to_snake_case "Some_uppercased_snake_case_string");
  [%expect {| some_uppercased_snake_case_string |}]
;;

let of_raw_string ?namespace raw_string =
  (* List of OCaml reserved keywords *)
  let keywords =
    [ "and"
    ; "as"
    ; "assert"
    ; "asr"
    ; "begin"
    ; "class"
    ; "constraint"
    ; "do"
    ; "done"
    ; "downto"
    ; "else"
    ; "end"
    ; "exception"
    ; "external"
    ; "false"
    ; "for"
    ; "fun"
    ; "function"
    ; "functor"
    ; "if"
    ; "in"
    ; "include"
    ; "inherit"
    ; "initializer"
    ; "land"
    ; "lazy"
    ; "let"
    ; "lor"
    ; "lsl"
    ; "lsr"
    ; "lxor"
    ; "match"
    ; "method"
    ; "mod"
    ; "module"
    ; "mutable"
    ; "new"
    ; "nonrec"
    ; "object"
    ; "of"
    ; "open"
    ; "or"
    ; "private"
    ; "rec"
    ; "sig"
    ; "struct"
    ; "then"
    ; "to"
    ; "true"
    ; "try"
    ; "type"
    ; "val"
    ; "virtual"
    ; "when"
    ; "while"
    ; "with"
    ]
  in
  let transform_char_if_illegal c =
    let is_legal = Char.is_alphanum c || Char.equal c '_' in
    if is_legal
    then String.of_char c
    else (
      match c with
      | '+' -> "plus"
      | '/' -> "slash"
      | _ -> "_")
  in
  let str_no_illegal_chars =
    String.to_list raw_string |> List.map ~f:transform_char_if_illegal |> String.concat
  in
  let is_invalid_first_char c = Char.is_digit c || Char.equal '\'' c in
  (* Convert CamelCase to snake_case *)
  let snake_case_str = to_snake_case str_no_illegal_chars in
  (* Prepend prefix for some cases where the variant name would start with an illegal
     character. *)
  let extra_prefix =
    match snake_case_str with
    | "" -> "empty_string_"
    | s when is_invalid_first_char (String.get s 0) -> "choice_"
    | s when String.for_all s ~f:(Char.equal '_') -> "dashes_only_"
    | _ -> ""
  in
  let extra_suffix =
    if List.mem keywords snake_case_str ~equal:String.equal then "_" else ""
  in
  let sanitized_string = extra_prefix ^ snake_case_str ^ extra_suffix in
  { raw = raw_string; sanitized = sanitized_string; namespace }
;;

let of_operation_path raw_string =
  let tr = String.tr_multi ~target:"/-{}$.#" ~replacement:"_" |> unstage in
  let sanitized =
    tr raw_string
    |> String.strip ~drop:(Char.equal '_')
    |> String.lowercase
    |> drop_consecutive_underscores
  in
  { sanitized; raw = raw_string; namespace = None }
;;

let to_module_name { sanitized; namespace; _ } =
  let leaf_module = String.capitalize sanitized in
  match namespace with
  | None -> leaf_module
  | Some namespace -> String.capitalize namespace ^ "." ^ leaf_module
;;

let to_variant { sanitized; _ } = String.capitalize sanitized
let to_variable_name { sanitized; _ } = sanitized
let to_raw_string { raw; _ } = raw

let to_truncated_name { sanitized; _ } =
  if String.length sanitized > 100
  then Md5_lib.string sanitized |> Md5_lib.to_hex |> String.append "truncated_"
  else sanitized
;;

let filenames_equal a b = String.(to_truncated_name a = to_truncated_name b)

include Comparable.Make (T)
