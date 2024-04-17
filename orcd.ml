module Let_syntax = struct
  let ( let+ ) x f =
    let open Cmdliner.Term in
    const f $ x

  let ( and+ ) tx ty =
    let pair x y = (x, y) in
    let open Cmdliner.Term in
    const pair $ tx $ ty
end

module Http = struct
  let get url =
    match Curly.get ~follow_redirects:true url with
    | Ok { code = 200; body; _ } -> body
    | Ok { code; _ } -> Printf.ksprintf failwith "Got error code %d" code
    | Error _ -> assert false
end

module Failure = struct
  type t = { package : string; reason : string option }

  let lint_str = "(lint)"

  let parse_re =
    let open Re in
    let package =
      rep1 (alt [ alnum; char '-'; char '.'; char '_'; char '+'; char '~' ])
    in
    compile
      (seq
         [
           bos;
           group (alt [ package; str lint_str ]);
           opt (seq [ str " (failed: "; group (rep1 any); str ")" ]);
           eos;
         ])

  let parse s =
    match Re.exec_opt parse_re s with
    | None -> Printf.ksprintf failwith "Error parsing %S" s
    | Some g ->
        let package = Re.Group.get g 1 in
        if String.equal package lint_str then None
        else
          let reason = Re.Group.get_opt g 2 in
          Some { package; reason }

  let write oc { package; reason } =
    match reason with
    | None -> Printf.fprintf oc "%s\n" package
    | Some r -> Printf.fprintf oc "%s (failed: %s)\n" package r
end

module Failures = struct
  module StringMap = Map.Make (String)

  let of_list l =
    List.fold_left
      (fun acc (f : Failure.t) -> StringMap.add f.package f acc)
      StringMap.empty l

  let write oc m = StringMap.iter (fun _ f -> Failure.write oc f) m

  let input_lines ic =
    let rec go acc =
      match In_channel.input_line ic with
      | Some s -> go (s :: acc)
      | None -> acc
    in
    go []

  let from_file s =
    In_channel.with_open_bin s input_lines
    |> List.filter_map (fun line ->
           if String.starts_with ~prefix:"#" line then None
           else Failure.parse line)
    |> of_list

  let diff a b = StringMap.fold (fun k _ acc -> StringMap.remove k acc) b a
  let cardinal = StringMap.cardinal
  let iter f = StringMap.iter (fun _ x -> f x)

  let concat = function
    | [] -> StringMap.empty
    | h :: t ->
        List.fold_left
          (fun acc m -> StringMap.union (fun _ v _ -> Some v) acc m)
          h t
end

module Scraper = struct
  let results_of_html html =
    let open Soup.Infix in
    Soup.parse html $ ".statuses" $$ ".failed" |> Soup.to_list
    |> List.filter_map (fun soup ->
           let text = Soup.R.leaf_text soup in
           Failure.parse text)
    |> Failures.of_list

  let active_count html =
    let open Soup.Infix in
    Soup.parse html $$ ".active" |> Soup.count
end

module Import = struct
  let info = Cmdliner.Cmd.info "import"

  let term =
    let open Let_syntax in
    let+ source =
      let open Cmdliner.Arg in
      required & pos 0 (some string) None & info []
    and+ html =
      let open Cmdliner.Arg in
      value & flag & info [ "html" ]
    and+ out =
      let open Cmdliner.Arg in
      value & opt (some string) None & info [ "o" ]
    in
    let html =
      if html then In_channel.with_open_bin source In_channel.input_all
      else Http.get source
    in
    let with_out f =
      match out with
      | Some path -> Out_channel.with_open_bin path (fun oc -> f oc)
      | None -> f Stdlib.stdout
    in
    with_out (fun oc -> html |> Scraper.results_of_html |> Failures.write oc)

  let cmd = Cmdliner.Cmd.v info term
end

module Diff = struct
  let info = Cmdliner.Cmd.info "diff"

  let term =
    let open Let_syntax in
    let arg n =
      let open Cmdliner.Arg in
      required & pos n (some string) None & info []
    in
    let+ path_a = arg 0
    and+ path_b = arg 1
    and+ paths_overrides =
      let open Cmdliner.Arg in
      value & opt_all string [] & info [ "overrides" ]
    in
    let a = Failures.from_file path_a in
    let b = Failures.from_file path_b in
    let overrides =
      List.map Failures.from_file paths_overrides |> Failures.concat
    in
    let diff = Failures.diff b a in
    let result = Failures.diff diff overrides in
    Printf.printf "New failures: %d\n" (Failures.cardinal result);
    Failures.iter (fun f -> Printf.printf "%a" Failure.write f) result

  let cmd = Cmdliner.Cmd.v info term
end

module Monitor = struct
  let info = Cmdliner.Cmd.info "monitor"

  let term =
    let open Let_syntax in
    let+ sources =
      let open Cmdliner.Arg in
      value & pos_all string [] & info []
    in
    let rec loop () =
      let total =
        List.fold_left
          (fun acc source ->
            let resp = Http.get source in
            let count = Scraper.active_count resp in
            Printf.printf "%s: %d\n" source count;
            acc + count)
          0 sources
      in
      if total = 0 then ()
      else (
        Unix.sleep 60;
        print_newline ();
        loop ())
    in
    loop ()

  let cmd = Cmdliner.Cmd.v info term
end

let info = Cmdliner.Cmd.info "opam-repo-ci-diff"
let cmd = Cmdliner.Cmd.group info [ Import.cmd; Diff.cmd; Monitor.cmd ]
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
