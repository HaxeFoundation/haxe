(*
	-D dump-fingerprints=<file>: the fingerprints of the typed entities of the
	compilation and of the parsed declarations, one per line and sorted
	(docs/incremental-compilation.md, phase 4). Comparing the files of two
	compilations tells which nodes changed.
*)
open Globals
open Type

let dump com file =
	let lines = ref [] in
	let add key node fp = lines := Printf.sprintf "%s\t%s\t%s" key node (TFingerprint.to_string fp) :: !lines in
	List.iter (fun m ->
		EntityKey.iter_module (fun key e ->
			let k = EntityKey.to_string key in
			match e with
			| EntityKey.EType mt ->
				add k "header" (TFingerprint.type_header mt);
				add k "members" (TFingerprint.type_members mt)
			| EntityKey.EField (c,cf) ->
				add k "sig" (TFingerprint.field_sig cf);
				if TFingerprint.has_inline_body c cf then add k "inline" (TFingerprint.field_inline_body cf);
				add k "body" (TFingerprint.field_body cf)
			| EntityKey.EAnonField (_,cf) ->
				add k "sig" (TFingerprint.field_sig cf)
			| EntityKey.EEnumField _ | EntityKey.ETypeParam _ ->
				()
		) m
	) com.Common.modules;
	Hashtbl.iter (fun file l ->
		List.iter (fun (name,fp) -> add (file ^ ":" ^ name) "decls" fp) l
	) TypeloadParse.decl_fingerprints;
	let ch = open_out_bin file in
	List.iter (fun l -> output_string ch l; output_char ch '\n') (List.sort compare !lines);
	close_out ch

let maybe_dump com =
	match Common.defined_value_safe com Define.DumpFingerprints with
	| "" -> ()
	| file -> Timer.time com.Common.timer_ctx ["fingerprints"] (dump com) file
