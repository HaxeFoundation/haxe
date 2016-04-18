open Ast
open Common
open Type

exception DocumentSymbols of string
exception DisplayTypes of t list
exception DisplayPosition of Ast.pos list

let is_display_file p =
	Common.unique_full_path p.pfile = (!Parser.resume_display).pfile

let encloses_position p_target p =
	p.pmin <= p_target.pmin && p.pmax >= p_target.pmax

let find_enclosing com e =
	let display_pos = ref (!Parser.resume_display) in
	let mk_null p = (EDisplay(((EConst(Ident "null")),p),false),p) in
	let encloses_display_pos p =
		if encloses_position !display_pos p then begin
			let p = !display_pos in
			display_pos := { pfile = ""; pmin = -2; pmax = -2 };
			Some p
		end else
			None
	in
	let rec loop e = match fst e with
		| EBlock el ->
			let p = pos e in
			(* We want to find the innermost block which contains the display position. *)
			let el = List.map loop el in
			let el = match encloses_display_pos p with
				| None ->
					el
				| Some p2 ->
					let b,el = List.fold_left (fun (b,el) e ->
						let p = pos e in
						if b || p.pmax <= p2.pmin then begin
							(b,e :: el)
						end else begin
							let e_d = (EDisplay(mk_null p,false)),p in
							(true,e :: e_d :: el)
						end
					) (false,[]) el in
					let el = if b then
						el
					else begin
						mk_null p :: el
					end in
					List.rev el
			in
			(EBlock el),(pos e)
		| _ ->
			Ast.map_expr loop e
	in
	loop e

let find_before_pos com e =
	let display_pos = ref (!Parser.resume_display) in
	let is_annotated p =
		if p.pmin <= !display_pos.pmin && p.pmax >= !display_pos.pmax then begin
			display_pos := { pfile = ""; pmin = -2; pmax = -2 };
			true
		end else
			false
	in
	let rec loop e =
		if is_annotated (pos e) then
			(EDisplay(e,false),(pos e))
		else
			e
	in
	let rec map e =
		loop (Ast.map_expr map e)
	in
	map e

module SymbolKind = struct
	type t =
		| File
		| Module
		| Namespace
		| Package
		| Class
		| Method
		| Property
		| Field
		| Constructor
		| Enum
		| Interface
		| Function
		| Variable
		| Constant
		| String
		| Number
		| Boolean
		| Array

	let to_int = function
		| File -> 1
		| Module -> 2
		| Namespace -> 3
		| Package -> 4
		| Class -> 5
		| Method -> 6
		| Property -> 7
		| Field -> 8
		| Constructor -> 9
		| Enum -> 10
		| Interface -> 11
		| Function -> 12
		| Variable -> 13
		| Constant -> 14
		| String -> 15
		| Number -> 16
		| Boolean -> 17
		| Array -> 18
end

module SymbolInformation = struct
	type t = {
		name : string;
		kind : SymbolKind.t;
		location : pos;
		containerName : string option;
	}

	let make name kind location containerName = {
		name = name;
		kind = kind;
		location = location;
		containerName = containerName;
	}
end

open Json
open SymbolKind
open SymbolInformation

let print_document_symbols (pack,decls) =
	let l = DynArray.create() in
	let add name kind location parent =
		let si = SymbolInformation.make name kind location (match parent with None -> None | Some si -> Some si.name) in
		DynArray.add l si;
		si
	in
(* 		let si_pack = match pack with
		| [] -> None
		| _ -> Some (add (String.concat "." pack) Package null_pos None) (* TODO: we don't have the position *)
	in *)
	let si_pack = None in (* TODO: no position, no point *)
	let rec expr si (e,p) =
		let add name kind location = add name kind location si in
		let add_ignore name kind location = ignore(add name kind location) in
		begin match e with
			(* TODO: disabled for now because it's too spammy *)
(* 			| EConst ct ->
			begin match ct with
				| Int i | Float i -> add_ignore i Number p
				| Ast.String s -> add_ignore s String p
				| Ident ("true" | "false" as s) -> add_ignore s Boolean p
				| Ident _ -> (* Hmm... *) ()
				| _ -> ()
			end *)
		| EVars vl ->
			List.iter (fun (s,_,eo) ->
				add_ignore s Variable p; (* TODO: Don't have a good pos here... *)
				expr_opt si eo
			) vl
		| ETry(e1,catches) ->
			expr si e1;
			List.iter (fun (s,_,e) ->
				add_ignore s Variable (pos e); (* TODO: No good pos as usual... *)
				expr si e
			) catches;
		| EFunction(Some s,f) ->
			let si_function = add s Function p in
			func si_function f
		| EIn((EConst(Ident s),p),e2) ->
			add_ignore s Variable p;
			expr si e2;
		| _ ->
			iter_expr (expr si) (e,p)
		end
	and expr_opt si eo = match eo with
		| None -> ()
		| Some e -> expr si e
	and func si f =
		List.iter (fun (s,_,_,eo) ->
			let si_arg = add s Variable si.location (* TODO: don't have *) (Some si) in
			expr_opt (Some si_arg) eo
		) f.f_args;
		expr_opt (Some si) f.f_expr
	in
	let field si_type cff = match cff.cff_kind with
		| FVar(_,eo) ->
			let si_field = add cff.cff_name Field cff.cff_pos (Some si_type) in
			expr_opt (Some si_field) eo
		| FFun f ->
			let si_method = add cff.cff_name (if cff.cff_name = "new" then Constructor else Method) cff.cff_pos (Some si_type) in
			func si_method f
		| FProp(_,_,_,eo) ->
			let si_property = add cff.cff_name Property cff.cff_pos (Some si_type) in
			expr_opt (Some si_property) eo
	in
	List.iter (fun (td,p) -> match td with
		| EImport _ | EUsing _ ->
			() (* TODO: Can we do anything with these? *)
		| EClass d ->
			let si_type = add d.d_name (if List.mem HInterface d.d_flags then Interface else Class) p si_pack in
			List.iter (field si_type) d.d_data
		| EEnum d ->
			let si_type = add d.d_name Enum p si_pack in
			List.iter (fun ef ->
				ignore (add ef.ec_name Method ef.ec_pos (Some si_type))
			) d.d_data
		| ETypedef d ->
			(* TODO: hmm... *)
			()
		| EAbstract d ->
			let si_type = add d.d_name Class p si_pack in
			List.iter (field si_type) d.d_data
	) decls;
	let error_printer file line = Printf.sprintf "%s:%d:" (Common.unique_full_path file) line in
	let jl = List.map (fun si ->
		let l =
			("name",JString si.name) ::
			("kind",JInt (to_int si.kind)) ::
			("location",JString (Lexer.get_error_pos error_printer si.location)) ::
			(match si.containerName with None -> [] | Some s -> ["containerName",JString s])
		in
		JObject l
	) (DynArray.to_list l) in
	let js = JArray jl in
	let b = Buffer.create 0 in
	write_json (Buffer.add_string b) js;
	Buffer.contents b