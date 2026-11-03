open! Core

module Maximum_filename_length = struct
  type t = int [@@deriving sexp, equal]

  let smallest_possible_value = 32 (* Things can get pretty weird otherwise. *)

  let of_int_exn t =
    if t < smallest_possible_value
    then
      failwith
        [%string
          "Maximum filename length cannot be smaller than \
           %{smallest_possible_value#Int}, got %{t#Int}."];
    t
  ;;

  let t_of_sexp sexp = t_of_sexp sexp |> of_int_exn
  let arg_type = Command.Arg_type.map Command.Arg_type.Export.int ~f:of_int_exn

  let roundtrippable_arg_type =
    Roundtrippable_arg_type.create
      ~arg_type
      ~to_string:Int.to_string
      ~arg_placeholder:"character-count"
  ;;

  let default = 255
end

module T = struct
  type t =
    { sanitized : string
    ; raw : string
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

(* Escapes a value-level identifier (a variable, record field, or function argument) that
   collides with an OCaml keyword by appending an underscore (e.g. [open] -> [open_]).
   This is only used for lowercase identifiers; capitalized names (variant constructors,
   module names) and file names are never keyword-escaped. *)
let escape_if_ocaml_keyword string =
  if Astlib.Keyword.is_keyword string then string ^ "_" else string
;;

let of_raw_string raw_string =
  let transform_char_if_illegal c =
    let is_legal = Char.is_alphanum c || Char.equal c '_' in
    if is_legal
    then String.of_char c
    else (
      match c with
      | '+' -> "plus"
      | '/' -> "slash"
      | ',' -> "comma"
      | '|' -> "pipe"
      | ';' -> "semicolon"
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
  let sanitized_string = extra_prefix ^ snake_case_str in
  { raw = raw_string; sanitized = sanitized_string }
;;

let of_operation_path raw_string =
  let tr = String.tr_multi ~target:"/-{}$.#" ~replacement:"_" |> unstage in
  let sanitized =
    tr raw_string
    |> String.strip ~drop:(Char.equal '_')
    |> String.lowercase
    |> drop_consecutive_underscores
  in
  let sanitized = if String.is_empty sanitized then "top_level_" else sanitized in
  { sanitized; raw = raw_string }
;;

let prepend_if_starts_with_underscore str =
  if String.is_prefix str ~prefix:"_" then "underscore" ^ str else str
;;

let capitalize str = prepend_if_starts_with_underscore str |> String.capitalize

let sanitize_maybe_truncate_module_name_uncapitalized sanitized ~maximum_filename_length =
  let sanitized = prepend_if_starts_with_underscore sanitized in
  let maximum_module_length = maximum_filename_length - String.length ".ml" in
  if String.length sanitized > maximum_module_length
  then (
    let hash = Md5_lib.string sanitized |> Md5_lib.to_hex in
    String.prefix sanitized (maximum_filename_length - String.length hash) ^ hash)
  else sanitized
;;

let to_module_name { sanitized; _ } ~kind =
  match kind with
  | `Full -> capitalize sanitized
  | `Truncated maximum_filename_length ->
    sanitize_maybe_truncate_module_name_uncapitalized sanitized ~maximum_filename_length
    |> String.capitalize
;;

(* Variant constructors and module names are capitalized, so they can never collide with
   an OCaml keyword and are deliberately not keyword-escaped. Only [to_variable_name]
   produces a lowercase value-level identifier that needs escaping. *)
let to_variant { sanitized; _ } = capitalize sanitized
let to_variable_name { sanitized; _ } = escape_if_ocaml_keyword sanitized
let to_raw_string { raw; _ } = raw

let to_filename { sanitized; _ } ~maximum_filename_length =
  sanitize_maybe_truncate_module_name_uncapitalized sanitized ~maximum_filename_length
  ^ ".ml"
;;

let filenames_equal a b ~maximum_filename_length =
  String.(to_filename ~maximum_filename_length a = to_filename ~maximum_filename_length b)
;;

include Comparable.Make (T)
