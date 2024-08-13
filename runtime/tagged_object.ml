open! Core
open! Async

let inject ~tag ~value = function
  | `Object lst ->
    `Object
      (List.map lst ~f:(fun (obj_key, obj_val) ->
         obj_key, if String.equal tag obj_key then `String value else obj_val))
  | _ -> failwith "failed to inject tag"
;;

let%expect_test "inject basic test" =
  let old_json =
    Jsonaf.of_string "{ \"state\" : \"creating\", \"example\" : \"text\" }"
  in
  let new_json = inject ~tag:"state" ~value:"TEST!!!" old_json in
  print_endline (Jsonaf.to_string old_json);
  print_endline (Jsonaf.to_string new_json);
  return
    [%expect
      {|
      {"state":"creating","example":"text"}
      {"state":"TEST!!!","example":"text"}
      |}]
;;

let retrieve ~tag json =
  match json with
  | `Object lst ->
    ( List.find lst ~f:(fun itm -> itm |> fst |> String.equal tag)
      |> Option.value_exn
      |> snd
      |> Jsonaf.string_exn
    , json )
  | _ -> failwith "no tag on received object, unable to parse"
;;

let%expect_test "retrieve basic test" =
  let tag, json =
    retrieve
      ~tag:"state"
      (Jsonaf.of_string "{ \"state\" : \"creating\", \"example\" : \"text\" }")
  in
  print_endline tag;
  print_endline (Jsonaf.to_string json);
  return
    [%expect
      {|
      creating
      {"state":"creating","example":"text"}
      |}]
;;
