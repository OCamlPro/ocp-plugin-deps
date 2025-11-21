(**************************************************************************)
(*                                                                        *)
(*   Typerex Tools                                                        *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 described in the file       *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)

open EzCompat
open Ez_toposort.V1
open Ez_call.V1

let verbose = ref false

let filename_concat dir file =
  match dir with
  | "" | "." -> file
  | _ -> Printf.sprintf "%s/%s" dir file

type lib = {
  lib_name : string ;
  lib_dir : string ;
  lib_plugin : string list ;
  lib_node : EZTOPOSORT.node ;
  lib_reqs : string list ;
  mutable lib_deps : lib list ;
}

module TOPO = EZTOPOSORT.MAKE(struct
    type t = lib
    let node lib = lib.lib_node
    let iter_edges f lib =
      List.iter f lib.lib_deps
    let name lib = lib.lib_name
  end)

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else
    let rec try_dir = function
        [] -> raise Not_found
      | dir::rem ->
          let fullname = filename_concat dir name in
          if !verbose then
            Printf.eprintf "CHECK %S?\n%!" fullname;
          if Sys.file_exists fullname then fullname
          else try_dir rem
    in
    try_dir path

let rec simplify_dir dirs file =
  match dirs with
  | [] -> file
  | dir :: dirs ->
      match EzString.chop_prefix file ~prefix:dir with
      | None -> simplify_dir dirs file
      | Some file ->
          let len = String.length file in
          if len > 0 && file.[0] = '/' then
            String.sub file 1 (len-1)
          else
            file

let opam_switch_prefix =
  match Sys.getenv "OPAM_SWITCH_PREFIX" with
  | exception Not_found -> None
  | dir -> Some dir

let main () =

  let lib, nolibs, includes =
    let anon_args = ref [] in
    let includes = ref [] in
    let arg_usage = "LIB [NOLIB]: display dependencies of LIB [that do \
                     no appear in dependencies of NOLIB]" in
    let arg_anon s = anon_args := s :: !anon_args in
    let arg_list = [
      "-I", Arg.String (fun dir ->
          includes := dir :: !includes), "<DIR> Search META in this directory";

      "-v", Arg.Set verbose,
      " Verbose mode";
    ] in
    Arg.parse arg_list arg_anon arg_usage;

    begin
      match opam_switch_prefix with
      | None -> ()
      | Some dir ->
          includes := (filename_concat dir "lib") :: !includes
    end;

    match List.rev !anon_args with
    | [] -> Arg.usage arg_list arg_usage ; exit 0
    | lib :: nolibs ->
        lib, nolibs, List.rev !includes
  in
  let ocamllib =
    match EZCALL.call_stdout_lines @@ [ "ocamlc" ; "-where" ] with
    | [ ocamllib ] -> ocamllib
    | _ -> assert false
  in
  let deps = ref StringMap.empty in
  let rec get_requires ?(retry=true) lib_name =
    match
      StringMap.find lib_name !deps
    with
    exception Not_found ->

        begin
          let lib_name,_ = EzString.cut_at lib_name '.' in
          let file =
            let file = Printf.sprintf "%s/META" lib_name in
            try
              find_in_path includes file
            with Not_found ->
              Printf.eprintf "Error: file %S not found in path\n%!" file;
              exit 2
          in
          if !verbose then
            Printf.eprintf "Loading %S\n%!" file;
          let t = MetaFile.meta_of_file file in

          let rec iter lib_name t =
            if !verbose then
              Printf.eprintf "  * adding %s\n%!" lib_name ;
            let reqs = MetaFile.requires t in
            if !verbose then begin
              Printf.eprintf "     Lib %S:\n" lib_name;
              Printf.eprintf "        Requires: \"%s\"\n%!"
                (String.concat "\" \"" reqs);
            end;
            let lib_dir = simplify_dir includes (Filename.dirname file) in
            let lib_dir =
              match MetaFile.directory t with
                "^" :: _ -> ocamllib
              | dir :: _ ->
                  let len = String.length dir in
                  if len > 0 && dir.[0] = '+' then
                    filename_concat ocamllib (String.sub dir 1 (len-1))
                  else
                    filename_concat lib_dir dir

              | _ -> lib_dir
            in
            let lib = {
              lib_name = lib_name ;
              lib_dir ;
              lib_plugin = MetaFile.plugin t
                  ~preds:MetaFile.preds_native;
              lib_reqs = (* List.map get_requires *) reqs ;
              lib_deps = [] ;
              lib_node = EZTOPOSORT.new_node ();
            } in
            deps := StringMap.add lib_name lib !deps;
            List.iter (fun (name, t) ->
                iter (Printf.sprintf "%s.%s" lib_name name) t
              ) t.p_packages;
          in
          iter lib_name t
        end;
        if retry then get_requires ~retry:false lib_name
        else begin
          Printf.eprintf "Error: lib %S not found\n%!" lib_name;
          exit 2
        end
    | lib ->
        begin
          match lib.lib_reqs, lib.lib_deps with
          | [], _ -> ()
          | reqs, [] ->
              lib.lib_deps <- List.map (fun lib_name ->
                  get_requires lib_name) reqs;
          | _ -> ()
        end;
        lib
  in
  let lib = get_requires lib in
  let nolibs = List.map (fun lib ->
      get_requires lib) nolibs in

  let all = StringMap.to_list !deps |> List.map snd in
  let sorted, cycles, unsorted = TOPO.sort all in

  assert (cycles = []);
  assert (unsorted = []);

  let skip_set = ref StringSet.empty in
  let rec skip lib =
    if not (StringSet.mem lib.lib_name !skip_set) then begin
      skip_set := StringSet.add lib.lib_name !skip_set ;
      List.iter skip lib.lib_deps
    end
  in
  List.iter skip nolibs;

  let needed_set = ref StringSet.empty in
  let rec need lib =
    if not (StringSet.mem lib.lib_name !needed_set) &&
       not (StringSet.mem lib.lib_name !skip_set)
    then begin
      needed_set := StringSet.add lib.lib_name !needed_set ;
      List.iter need lib.lib_deps
    end
  in
  List.iter need lib.lib_deps;

  Printf.printf "Plugin dependencies of %s:\n" lib.lib_name;
  begin
    match nolibs with
    | [] -> ()
    | nolibs ->
        Printf.printf "  Loaded after %s\n%!"
          (String.concat " "
             (List.map (fun lib -> lib.lib_name) nolibs))
  end;
  let counter = ref 0 in
  List.iter (fun lib ->
      if StringSet.mem lib.lib_name !needed_set then begin
        incr counter ;
        Printf.printf "   %2d. %-20s %s\n%!"
          !counter lib.lib_name
          (String.concat " "
             (List.map (fun name ->
                  Printf.sprintf "%s/%s" lib.lib_dir name) lib.lib_plugin))
      end
    ) sorted;

  ()
