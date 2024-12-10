(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)
open Globals
open Ast
open Reification
open Parser
open DisplayPosition

let popt f = function%parser
	| [ f as v ] -> Some v
	| [ ] -> None

let rec plist f = function%parser
	| [ f as v; [%let l = plist f] ] -> v :: l
	| [ ] -> []

let psep_nonempty sep f = function%parser
	| [ f as v; [%s s] ] ->
		let rec loop = function%parser
			| [ (sep2,_); f as v; loop as l ] when sep2 = sep -> v :: l
			| [ ] -> []
		in
		v :: loop s

let psep sep f = function%parser
	| [ [%let r = psep_nonempty sep f] ] -> r
	| [ ] -> []

let rec psep_trailing sep f = function%parser
	| [ f as v; [%s s] ] ->
		begin match%parser s with
		| [ (sep2,_); [%let l = psep_trailing sep f] ] when sep2 = sep -> v :: l
		| [ ] -> [v]
		end
	| [ ] -> []

let pignore f =
	try
		ignore(f())
	with Stream.Error _ | Stream.Failure ->
		()

let expect_unless_resume_p t s = match Stream.peek s with
	| Some (t',p) when t' = t ->
		Stream.junk s;
		p
	| _ ->
		syntax_error (Expected [s_token t]) s (next_pos s)

let ident = function%parser
	| [ (Const (Ident i),p) ] -> i,p

let dollar_ident = function%parser
	| [ (Const (Ident i),p) ] -> i,p
	| [ (Dollar i,p) ] -> ("$" ^ i),p

let dollar_ident_macro pack = function%parser
	| [ (Const (Ident i),p) ] -> i,p
	| [ (Dollar i,p) ] -> ("$" ^ i),p
	| [ (Kwd Macro,p) ] when pack <> [] -> "macro", p
	| [ (Kwd Extern,p) ] when pack <> [] -> "extern", p
	| [ (Kwd Function,p) ] when pack <> [] -> "function", p

let lower_ident_or_macro = function%parser
	| [ (Const (Ident i),p) ] when is_lower_ident i -> i
	| [ (Kwd Macro,_) ] -> "macro"
	| [ (Kwd Extern,_) ] -> "extern"
	| [ (Kwd Function,_) ] -> "function"

let property_ident = function%parser
	| [ ident as i ] -> i
	| [ (Kwd Dynamic,p) ] -> "dynamic",p
	| [ (Kwd Default,p) ] -> "default",p
	| [ (Kwd Null,p) ] -> "null",p

let questionable_dollar_ident s =
	let po = match%parser s with
		| [ (Question,p) ] -> Some p
		| [ ] -> None
	in
	let name,p = dollar_ident s in
	match po with
		| None ->
			false,(name,p)
		| Some p' ->
			if p.pmin <> p'.pmax then syntax_error (Custom (Printf.sprintf "Invalid usage of ?, use ?%s instead" name)) s ~pos:(Some p') ();
			true,(name,p)

let question_mark = function%parser
	| [ (Question,p) ] -> p

let semicolon s =
	if fst (last_token s) = BrClose then
		match%parser s with
		| [ (Semicolon,p) ] -> p
		| [ ] -> snd (last_token s)
	else
		match%parser s with
		| [ (Semicolon,p) ] -> p
		| [ ] ->
			syntax_error Missing_semicolon s (next_pos s)

let check_redundant_var p1 = function%parser
	| [ (Kwd Var),p2; [%s s] ] ->
		syntax_error (Custom "`final var` is not supported, use `final` instead") ~pos:(Some (punion p1 p2)) s ();
	| [ ] ->
		()

let parsing_macro_cond = ref false

let rec parse_file s =
	last_doc := None;
	match%parser s with
	| [ (Kwd Package,_); parse_package as pack ] ->
		begin match%parser s with
		| [ (Const(Ident _),p) ] when pack = [] -> error (Custom "Package name must start with a lowercase character") p
		| [ semicolon as psem; [%let l = parse_type_decls TCAfterImport psem.pmax pack []] ] -> pack , l
		end
	| [ [%let l = parse_type_decls TCBeforePackage (-1) [] []] ] -> [] , l

and parse_type_decls mode pmax pack acc s =
	check_type_decl_completion mode pmax s;
	let result = try
		begin match%parser s with
		| [ [%let cff = parse_type_decl mode] ] -> Success cff
		| [ (Eof,p) ] -> End p
		| [ ] -> Error "Parse error."
		end
	with
	| TypePath ([],Some (name,false),b,p) ->
		(* resolve imports *)
		List.iter (fun d ->
			match fst d with
			| EImport (t,_) ->
				(match List.rev t with
				| (n,_) :: path when n = name && List.for_all (fun (i,_) -> is_lower_ident i) path -> raise (TypePath (List.map fst (List.rev path),Some (name,false),b,p))
				| _ -> ())
			| _ -> ()
		) acc;
		raise (TypePath (pack,Some(name,true),b,p))
	| Stream.Error msg when !in_display_file ->
		Error msg
	in
	match result with
	| Success (td,p) ->
		let mode = match td with
			| EImport _ | EUsing _ -> TCAfterImport
			| _ -> TCAfterType
		in
		parse_type_decls mode p.pmax pack ((td,p) :: acc) s
	| End _ ->
		List.rev acc
	| Error msg ->
		handle_stream_error msg s;
		ignore(resume false false s);
		parse_type_decls mode (last_pos s).pmax pack acc s

and parse_abstract doc meta flags p1 = function%parser
	| [ type_name as name; parse_constraint_params as tl; parse_abstract_subtype as st; [%let sl = plist parse_abstract_relations]; [%s s] ] ->
		let fl,p2 = match%parser s with
			| [ (BrOpen,_); [%let fl, p2 = parse_class_fields false p1] ] -> fl,p2
			| [ ] -> syntax_error (Expected ["{";"to";"from"]) s ([],last_pos s)
		in
		let flags = (match st with None -> flags | Some t -> AbOver t :: flags) in
		({
			d_name = name;
			d_doc = doc_from_string_opt doc;
			d_meta = meta;
			d_params = tl;
			d_flags = flags @ sl;
			d_data = fl;
		},punion p1 p2)

and parse_class_content doc meta flags n p1 s =
	let name = type_name s in
	let tl = parse_constraint_params s in
	let rec loop had_display p0 acc =
		let check_display p1 =
			if not had_display && !in_display_file && !display_mode = DMDefault && display_position#enclosed_in p1 then
				syntax_completion (if List.mem HInterface n then SCInterfaceRelation else SCClassRelation) None (display_position#with_pos p1)
		in
		match%parser s with
		| [ (Kwd Extends,p1); [%let ptp,b = parse_type_path_or_resume p1] ] ->
			check_display {p1 with pmin = p0.pmax; pmax = p1.pmin};
			let p0 = ptp.pos_full in
			(* If we don't have type parameters, we have to offset by one so to not complete `extends`
				and `implements` after the identifier. *)
			let p0 = {p0 with pmax = p0.pmax + (if ptp.path.tparams = [] then 1 else 0)} in
			loop (had_display || b) p0 ((HExtends ptp) :: acc)
		| [ (Kwd Implements,p1); [%let ptp,b = parse_type_path_or_resume p1] ] ->
			check_display {p1 with pmin = p0.pmax; pmax = p1.pmin};
			let p0 = ptp.pos_full in
			let p0 = {p0 with pmax = p0.pmax + (if ptp.path.tparams = [] then 1 else 0)} in
			loop (had_display || b) p0 ((HImplements ptp) :: acc)
		| [ (BrOpen,p1) ] ->
			check_display {p1 with pmin = p0.pmax; pmax = p1.pmin};
			List.rev acc
		| [ ] ->
			begin match Stream.peek s with
			| Some((Const(Ident name),p)) when display_position#enclosed_in p ->
				syntax_completion (if List.mem HInterface n then SCInterfaceRelation else SCClassRelation) (Some name) p
			| _ ->
				check_display {p1 with pmin = p0.pmax; pmax = (next_pos s).pmax};
				syntax_error (Expected ["extends";"implements";"{"]) s (List.rev acc)
			end
	in
	let hl = loop false (last_pos s) [] in
	let fl, p2 = parse_class_fields false p1 s in
	(EClass {
		d_name = name;
		d_doc = doc_from_string_opt doc;
		d_meta = meta;
		d_params = tl;
		d_flags = ExtList.List.filter_map decl_flag_to_class_flag flags @ n @ hl;
		d_data = fl;
	}, punion p1 p2)

and parse_type_decl mode s =
	match%parser s with
	| [ (Kwd Import,p1) ] -> parse_import s p1
	| [ (Kwd Using,p1) ] -> parse_using s p1
	| [ get_doc as doc; parse_meta as meta; parse_common_flags as c ] ->
		match%parser s with
		| [ (Kwd Function,p1); dollar_ident as name; parse_constraint_params as pl; (POpen,_); [%let args = psep_trailing Comma parse_fun_param]; (PClose,_); [%let t = popt parse_type_hint] ] ->
			let e, p2 = (match%parser s with
				| [ expr as e ] ->
					ignore(semicolon s);
					Some e, pos e
				| [ semicolon as p ] -> None, p
				| [ ] -> serror()
			) in
			let f = {
				f_params = pl;
				f_args = args;
				f_type = t;
				f_expr = e;
			} in
			(EStatic {
				d_name = name;
				d_doc = doc_from_string_opt doc;
				d_meta = meta;
				d_params = pl;
				d_flags = ExtList.List.filter_map decl_flag_to_module_field_flag c;
				d_data = FFun f;
			}, punion p1 p2)
		| [ (Kwd Var,p1); dollar_ident as name ] ->
			let p2,t =
				match%parser s with
				| [ (POpen,_); property_ident as i1; (Comma,_); property_ident as i2; (PClose,_) ] ->
					let t = popt parse_type_hint s in
					let e,p2 = parse_var_field_assignment s in
					p2,FProp (i1,i2,t,e)
				| [ [%let t = popt parse_type_hint] ] ->
					let e,p2 = parse_var_field_assignment s in
					p2,FVar (t,e)
			in
			(EStatic {
				d_name = name;
				d_doc = doc_from_string_opt doc;
				d_meta = meta;
				d_params = [];
				d_flags = ExtList.List.filter_map decl_flag_to_module_field_flag c;
				d_data = t;
			}, punion p1 p2)
		| [ (Kwd Enum,p1) ] ->
			begin match%parser s with
			| [ (Kwd Abstract,p1); [%let a,p = parse_abstract doc meta (AbEnum :: (convert_abstract_flags c)) p1] ] ->
				(EAbstract a,p)
			| [ type_name as name; parse_constraint_params as tl; (BrOpen,_); [%let l = plist parse_enum]; (BrClose,p2) ] ->
				(EEnum {
					d_name = name;
					d_doc = doc_from_string_opt doc;
					d_meta = meta;
					d_params = tl;
					d_flags = ExtList.List.filter_map decl_flag_to_enum_flag c;
					d_data = l
				}, punion p1 p2)
			end
		| [ [%let n, p1 = parse_class_flags] ] ->
			parse_class_content doc meta c n p1 s
		| [ (Kwd Typedef,p1); type_name as name; parse_constraint_params as tl; (Binop OpAssign,p2); [%let t = parse_complex_type_at p2] ] ->
			(match%parser s with
			| [ (Semicolon,_) ] -> ()
			| [ ] -> ());
			(ETypedef {
				d_name = name;
				d_doc = doc_from_string_opt doc;
				d_meta = meta;
				d_params = tl;
				d_flags = ExtList.List.filter_map decl_flag_to_typedef_flag c;
				d_data = t;
			}, punion p1 (pos t))
		| [ (Kwd Abstract,p1) ] ->
			begin match%parser s with
			| [ [%let a,p = parse_abstract doc meta (convert_abstract_flags c) p1] ] ->
				EAbstract a,p
			| [ ] ->
				let c2 = parse_common_flags s in
				begin match%parser s with
				| [ [%let flags,_p = parse_class_flags ] ] ->
					parse_class_content doc meta (c @ c2) (HAbstract :: flags) p1 s
				| [ ] ->
					serror()
				end
			end
		| [ ] ->
			match List.rev c with
			| (DFinal,p1) :: crest ->
				(match%parser s with
				| [ dollar_ident as name; [%let t = popt parse_type_hint]; [%let e,p2 = parse_var_field_assignment] ] ->
					(EStatic {
						d_name = name;
						d_doc = doc_from_string_opt doc;
						d_meta = meta;
						d_params = [];
						d_flags = (ExtList.List.filter_map decl_flag_to_module_field_flag (List.rev crest)) @ [AFinal,p1];
						d_data = FVar(t,e);
					}, punion p1 p2)
				| [ ] -> check_type_decl_flag_completion mode c s)
			| _ ->
				check_type_decl_flag_completion mode c s


and parse_class doc meta cflags need_name s =
	let opt_name = if need_name then type_name else (fun s -> match popt type_name s with None -> "",null_pos | Some n -> n) in
	match%parser s with
	| [ [%let n,p1 = parse_class_flags]; opt_name as name; parse_constraint_params as tl; [%let hl = plist parse_class_herit]; (BrOpen,_); [%let fl, p2 = parse_class_fields (not need_name) p1] ] ->
		(EClass {
			d_name = name;
			d_doc = doc;
			d_meta = meta;
			d_params = tl;
			d_flags = List.map fst cflags @ n @ hl;
			d_data = fl;
		}, punion p1 p2)

and parse_import' s p1 =
	let rec loop pn acc =
		match%parser s with
		| [ (Dot,p) ] ->
			let resume() =
				type_path (List.map fst acc) true (punion pn p)
			in
			check_resume p resume (fun () -> ());
			begin match%parser s with
			| [ (Const (Ident k),p) ] ->
				loop pn ((k,p) :: acc)
			| [ (Kwd Macro,p) ] ->
				loop pn (("macro",p) :: acc)
			| [ (Kwd Extern,p) ] ->
				loop pn (("extern",p) :: acc)
			| [ (Kwd Function,p) ] ->
				loop pn (("function",p) :: acc)
			| [ (Binop OpMult,_) ] ->
				List.rev acc, IAll
			| [ ] ->
				ignore(popt semicolon s);
				syntax_error (Expected ["identifier"]) s (List.rev acc,INormal)
			end
		| [ (Kwd In,_); (Const (Ident name),pname) ] ->
			List.rev acc, IAsName(name,pname)
		| [ (Const (Ident "as"),_); (Const (Ident name),pname) ] ->
			List.rev acc, IAsName(name,pname)
		| [ ] ->
			List.rev acc,INormal
	in
	let path, mode = (match%parser s with
		| [ (Const (Ident name),p) ] -> loop p [name,p]
		| [ ] ->
			if would_skip_display_position p1 true s then
				([],INormal)
			else
				syntax_error (Expected ["identifier"]) s ([],INormal)
	) in
	(path,mode)

and parse_import s p1 =
	let (path,mode) = parse_import' s p1 in
	let p2 = match%parser s with
	| [ (Semicolon,p2) ] ->
		p2
	| [ ] ->
		if would_skip_display_position p1 true s then
			display_position#with_pos p1
		else
			syntax_error (Expected [".";";";"as"]) s (last_pos s)
	in
	(EImport (path,mode),punion p1 p2)

and parse_using' s p1 =
	let rec loop pn acc =
		match%parser s with
		| [ (Dot,p) ] ->
			check_resume p (fun () -> type_path (List.map fst acc) false (punion pn p)) (fun () -> ());
			begin match%parser s with
			| [ (Const (Ident k),p) ] ->
				loop pn ((k,p) :: acc)
			| [ (Kwd Macro,p) ] ->
				loop pn (("macro",p) :: acc)
			| [ (Kwd Extern,p) ] ->
				loop pn (("extern",p) :: acc)
			| [ (Kwd Function,p) ] ->
				loop pn (("function",p) :: acc)
			| [ ] ->
				syntax_error (Expected ["identifier"]) s (List.rev acc);
			end
		| [ ] ->
			List.rev acc
	in
	match%parser s with
		| [ (Const (Ident name),p) ] -> loop p [name,p]
		| [ ] ->
			if would_skip_display_position p1 true s then
				[]
			else
				syntax_error (Expected ["identifier"]) s []

and parse_using s p1 =
	let path = parse_using' s p1 in
	let p2 = match%parser s with
	| [ (Semicolon,p2) ] ->
		p2
	| [ ] ->
		if would_skip_display_position p1 true s then
			display_position#with_pos p1
		else
			syntax_error (Expected [".";";"]) s (last_pos s)
	in
	(EUsing path,punion p1 p2)

and parse_abstract_relations =
	let check_display p1 (ct,p2) =
		if !in_display_file && p1.pmax < (display_position#get).pmin && p2.pmin >= (display_position#get).pmax then
			(* This means we skipped the display position between the to/from and the type-hint we parsed.
			   Very weird case, it was probably a {} like in #7137. Let's discard it and use magic. *)
			(magic_type_th (display_position#with_pos p2))
		else
			(ct,p2)
	in
	function%parser
	| [ (Const (Ident "to"),p1); [%let t = parse_complex_type_at p1] ] -> (AbTo (check_display p1 t))
	| [ (Const (Ident "from"),p1); [%let t = parse_complex_type_at p1] ] -> AbFrom (check_display p1 t)

and parse_abstract_subtype = function%parser
	| [ (POpen, _); parse_complex_type as t; (PClose,_) ] -> Some t
	| [ ] -> None

and parse_package = psep Dot lower_ident_or_macro

and resume tdecl fdecl s =
	(* look for next variable/function or next type declaration *)
	let rec junk k =
		if k <= 0 then () else begin
			Stream.junk s;
			junk (k - 1);
		end
	in
	(*
		walk back tokens which are prefixing a type/field declaration
	*)
	let rec junk_tokens k =
		if k = 0 then
			()
		else match List.rev_map fst (Stream.npeek k s) with
		| Kwd Private :: _ -> junk_tokens (k - 1)
		| (Const (Ident _) | Kwd _) :: DblDot :: At :: l
		| (Const (Ident _) | Kwd _) :: At :: l ->
			junk_tokens (List.length l)
		| PClose :: l ->
			(* count matching parenthesises for metadata call *)
			let rec loop n = function
				| [] -> []
				| POpen :: l -> if n = 0 then l else loop (n - 1) l
				| PClose :: l -> loop (n + 1) l
				| _ :: l -> loop n l
			in
			(match loop 0 l with
			| (Const (Ident _) | Kwd _) :: At :: l
			| (Const (Ident _) | Kwd _) :: DblDot :: At :: l -> junk_tokens (List.length l)
			| _ ->
				junk k)
		| _ ->
			junk k
	in
	let rec loop k =
		match List.rev_map fst (Stream.npeek k s) with
		(* metadata *)
		| Kwd _ :: At :: _ | Kwd _ :: DblDot :: At :: _ ->
			loop (k + 1)
		(* field declaration *)
		| Const _ :: Kwd Function :: _
		| Kwd New :: Kwd Function :: _ when fdecl ->
			junk_tokens (k - 2);
			true
		| Kwd Macro :: _ | Kwd Public :: _ | Kwd Static :: _ | Kwd Var :: _ | Kwd Final :: _ | Kwd Override :: _ | Kwd Dynamic :: _ | Kwd Inline :: _ | Kwd Overload :: _ when fdecl ->
			junk_tokens (k - 1);
			true
		| BrClose :: _ when tdecl ->
			junk_tokens (k - 1);
			false
		(* type declaration *)
		| Eof :: _ | Kwd Import :: _ | Kwd Using :: _ | Kwd Extern :: _ | Kwd Class :: _ | Kwd Interface :: _ | Kwd Enum :: _ | Kwd Typedef :: _ | Kwd Abstract :: _->
			junk_tokens (k - 1);
			false
		| [] ->
			false
		| _ ->
			loop (k + 1)
	in
	loop 1

and parse_class_field_resume acc tdecl s =
	let result = try
		begin match%parser s with
		| [ [%let cff = parse_class_field tdecl] ] -> Success cff
		| [ (BrClose,p) ] -> End p
		| [ ] -> Error "Parse error."
		end
	with Stream.Error msg ->
		Error msg
	in
	match result with
	| Success cff ->
		parse_class_field_resume (cff :: acc) tdecl s
	| End p ->
		List.rev acc,p
	| Error msg ->
		handle_stream_error msg s;
		if resume tdecl true s then
			parse_class_field_resume acc tdecl s
		else
			acc,last_pos s

and parse_class_fields tdecl p1 s =
	if not (!in_display_file) then begin
		let acc = plist (parse_class_field tdecl) s in
		let p2 = (match%parser s with
			| [ (BrClose,p2) ] -> p2
			| [ ] -> error (Expected ["}"]) (next_pos s)
		) in
		acc,p2
	end else
		parse_class_field_resume [] tdecl s

and parse_common_flags = function%parser
	| [ (Kwd Private,p); parse_common_flags as l ] -> (DPrivate,p) :: l
	| [ (Kwd Extern,p); parse_common_flags as l ] -> (DExtern,p) :: l
	| [ (Kwd Final,p); parse_common_flags as l ] -> (DFinal,p) :: l
	| [ (Kwd Macro,p); parse_common_flags as l ] -> (DMacro,p) :: l
	| [ (Kwd Dynamic,p); parse_common_flags as l ] -> (DDynamic,p) :: l
	| [ (Kwd Inline,p); parse_common_flags as l ] -> (DInline,p) :: l
	| [ (Kwd Public,p); parse_common_flags as l ] -> (DPublic,p) :: l
	| [ (Kwd Static,p); parse_common_flags as l ] -> (DStatic,p) :: l
	| [ (Kwd Overload,p); parse_common_flags as l ] -> (DOverload,p) :: l
	| [ ] -> []

and parse_meta_argument_expr s =
	let e = expr s in
	begin match fst e with
	| EDisplay(e1,DKDot) ->
		begin try
			type_path (string_list_of_expr_path_raise e1) false (pos e1)
		with Exit ->
			e
		end
	| _ ->
		e
	end

and parse_meta_params pname s = match%parser s with
	| [ (POpen,p); [%let params = psep_trailing Comma parse_meta_argument_expr]; ] when p.pmin = pname.pmax ->
		ignore(expect_unless_resume_p PClose s);
		params
	| [ ] -> []

and parse_meta_entry = function%parser
	[ (At,p1); [%s s] ] ->
		let meta = check_resume p1 (fun () -> Some (Meta.HxCompletion,[],p1)) (fun () -> None) in
		match%parser s with
		| [ [%let name,p = parse_meta_name p1]; [%let params = parse_meta_params p] ] -> (name,params,punion p1 p)
		| [ ] -> match meta with None -> serror() | Some meta -> meta

and parse_meta = function%parser
	| [ parse_meta_entry as entry; [%s s] ] ->
		entry :: parse_meta s
	| [ ] -> []

and parse_meta_name_2 p1 acc s =
	let part,p = match%parser s with
		| [ (Const (Ident i),p) ] when p.pmin = p1.pmax -> i,p
		| [ (Kwd k,p) ] when p.pmin = p1.pmax -> s_keyword k,p
	in
	let acc = part :: acc in
	match%parser s with
	| [ (Dot,p1); [%let part,p2 = parse_meta_name_2 p1 acc] ] -> part,punion p p2
	| [ ] -> acc,punion p1 p

and parse_meta_name p1 s = match%parser s with
	| [ (DblDot,p) ]  when p.pmin = p1.pmax ->
		let meta = check_resume p (fun () -> Some (Meta.HxCompletion,p)) (fun() -> None) in
		begin match%parser s with
		| [ [%let name,p2 = parse_meta_name_2 p []] ] -> (Meta.parse (rev_concat "." name)),p2
		| [ ] -> match meta with None -> raise Stream.Failure | Some meta -> meta
		end
	| [ [%let name,p2 = parse_meta_name_2 p1 []] ] -> (Meta.Custom (rev_concat "." name)),p2

and parse_enum_flags = function%parser
	| [ (Kwd Enum,p) ] -> [] , p

and parse_class_flags = function%parser
	| [ (Kwd Class,p) ] -> [] , p
	| [ (Kwd Interface,p) ] -> [HInterface] , p

and parse_complex_type_at p s = match%parser s with
	| [ parse_complex_type as t ] -> t
	| [ ] ->
		if would_skip_display_position p false s then
			(magic_type_th (display_position#with_pos p))
		else
			serror()

and parse_type_hint = function%parser
	| [ (DblDot,p1); [%s s] ] ->
		let f () = parse_complex_type_at p1 s in
		check_resume_range p1 s
			(fun p2 ->
				pignore(f);
				magic_type_th (display_position#with_pos p1)
			)
			f

and parse_type_opt = function%parser
	| [ parse_type_hint as t ] -> Some t
	| [ ] -> None

and parse_complex_type s = parse_complex_type_maybe_named false s

and parse_complex_type_maybe_named allow_named = function%parser
	| [ (POpen,p1); [%let tl = psep_trailing Comma (parse_complex_type_maybe_named true)]; (PClose,p2); [%s s] ] ->
		begin match tl with
		| [] | [(CTNamed _,_)] ->
			(* it was () or (a:T) - clearly a new function type syntax, proceed with parsing return type *)
			parse_function_type_next tl p1 s
		| [t] ->
			(* it was some single unnamed type in parenthesis - use old function type syntax  *)
			let t = CTParent t,punion p1 p2 in
			parse_complex_type_next t s
		| _ ->
			(* it was multiple arguments - clearly a new function type syntax, proceed with parsing return type  *)
			parse_function_type_next tl p1 s
		end
	| [ [%s s] ] ->
		let t = parse_complex_type_inner allow_named s in
		parse_complex_type_next t s

and parse_structural_extension = function%parser
	| [ (Binop OpGt,p1); [%s s] ] ->
		match%parser s with
		| [ parse_type_path as t ] ->
			begin match%parser s with
				| [ (Comma,_) ] -> t
				| [ ] -> syntax_error (Expected [","]) s t
			end;
		| [ ] ->
			if would_skip_display_position p1 false s then begin
				begin match%parser s with
					| [ (Comma,_) ] -> ()
					| [ ] -> ()
				end;
				let p = display_position#with_pos p1 in
				make_ptp magic_type_path p
			end else
				raise Stream.Failure

and parse_complex_type_inner allow_named s = match%parser s with
	| [ (POpen,p1); parse_complex_type as t; (PClose,p2) ] -> CTParent t,punion p1 p2
	| [ (BrOpen,p1) ] ->
		(match%parser s with
		| [ [%let l,p2 = parse_type_anonymous] ] -> CTAnonymous l,punion p1 p2
		| [ parse_structural_extension as t ] ->
			let tl = t :: plist parse_structural_extension s in
			(match%parser s with
			| [ [%let l,p2 = parse_type_anonymous] ] -> CTExtend (tl,l),punion p1 p2
			| [ [%let l,p2 = parse_class_fields true p1] ] -> CTExtend (tl,l),punion p1 p2)
		| [ [%let l,p2 = parse_class_fields true p1] ] -> CTAnonymous l,punion p1 p2
		| [ ] -> serror())
	| [ (Question,p1); [%let t,p2 = parse_complex_type_inner allow_named] ] ->
		CTOptional (t,p2),punion p1 p2
	| [ (Spread,p1); [%let t,p2 = parse_complex_type_inner allow_named] ] ->
		let hint =
			match t with
			| CTNamed (_,hint) -> hint
			| _ -> (t,p2)
		in
		let p = punion p1 p2 in
		CTPath (make_ptp (mk_type_path ~params:[TPType hint] (["haxe"],"Rest")) p),p
	| [ dollar_ident as n ] ->
		(match%parser s with
		| [ (DblDot,_); parse_complex_type as t ] when allow_named->
			let p1 = snd n in
			let p2 = snd t in
			CTNamed (n,t),punion p1 p2
		| [ [%s s] ] ->
			let n,p = n in
			let ptp = parse_type_path2 None [] n p s in
			CTPath ptp,ptp.pos_full)
	| [ parse_type_path as ptp ] ->
		CTPath ptp,ptp.pos_full

and parse_type_path s = parse_type_path1 None [] s

and parse_type_path1 p0 pack = function%parser
	| [ [%let name, p1 = dollar_ident_macro pack]; [%s s] ] ->
		parse_type_path2 p0 pack name p1 s

and parse_type_path2 p0 pack name p1 s : placed_type_path =
	let check_display f =
		let p = match p0 with
			| None -> p1
			| Some p -> punion p p1
		in
		if !in_display_file && display_position#enclosed_in p then begin
			make_ptp (mk_type_path (List.rev pack,name)) p
		end else
			f()
	in
	if is_lower_ident name then
		(match%parser s with
		| [ (Dot,p) ] ->
			check_resume p
				(fun () -> raise (TypePath (List.rev (name :: pack),None,false,punion (match p0 with None -> p1 | Some p0 -> p0) p)))
				(fun () -> parse_type_path1 (match p0 with None -> Some p1 | Some _ -> p0) (name :: pack) s)
		| [ (Semicolon,_) ] ->
			check_display (fun () -> error (Custom "Type name should start with an uppercase letter") p1)
		| [ ] ->
			check_display serror)
	else
		let sub,p2 = (match%parser s with
			| [ (Dot,p) ] ->
				(check_resume p
					(fun () -> raise (TypePath (List.rev pack,Some (name,false),false,punion (match p0 with None -> p1 | Some p0 -> p0) p)))
					(fun () -> match%parser s with
					| [ (Const (Ident name),p2) ] when not (is_lower_ident name) -> Some name,p2
					| [ ] -> serror()))
			| [ ] -> None,p1
		) in
		let p1 = match p0 with None -> p1 | Some p -> p in
		let p_path = punion p1 p2 in
		let params,p2 = (match%parser s with
			| [ (Binop OpLt,plt); [%let l = psep Comma (parse_type_path_or_const plt)] ] ->
				begin match%parser s with
				| [ (Binop OpGt,p2) ] -> l,p2
				| [ ] ->
					syntax_error (Expected [">"]) s (l,pos (last_token s))
				end
			| [ ] -> [],p2
		) in
		let tp = mk_type_path ~params ?sub (List.rev pack,name)
		and p_full = punion p1 p2 in
		make_ptp tp ~p_path p_full

and type_name = function%parser
	| [ (Const (Ident name),p); [%s s] ] ->
		if is_lower_ident name then
			syntax_error (Custom "Type name should start with an uppercase letter") ~pos:(Some p) s (name,p)
		else
			name,p
	| [ (Dollar name,p) ] -> "$" ^ name,p

and parse_type_path_or_const plt = function%parser
	(* we can't allow (expr) here *)
	| [ (BkOpen,p1); [%let e = parse_array_decl p1] ] -> TPExpr (e)
	| [ parse_complex_type as t ] -> TPType t
	| [ (Unop op,p1); (Const c,p2) ] -> TPExpr (make_unop op (EConst c,p2) p1)
	| [ (Binop OpSub,p1); (Const c,p2) ] -> TPExpr (make_unop Neg (EConst c,p2) p1)
	| [ (Const c,p) ] -> TPExpr (EConst c,p)
	| [ (Kwd True,p) ] -> TPExpr (EConst (Ident "true"),p)
	| [ (Kwd False,p) ] -> TPExpr (EConst (Ident "false"),p)
	| [ expr as e ] -> TPExpr e
	| [ [%s s] ] ->
		if !in_display_file then begin
			if would_skip_display_position plt false s then begin
				TPType (magic_type_th (display_position#with_pos plt))
			end else
				raise Stream.Failure
		end else
			serror()

and parse_complex_type_next (t : type_hint) s =
	let make_fun t2 p2 = match t2 with
		| CTFunction (args,r) ->
			CTFunction (t :: args,r),punion (pos t) p2
		| _ ->
			CTFunction ([t] , (t2,p2)),punion (pos t) p2
	in
	let make_intersection t2 p2 = match t2 with
		| CTIntersection tl ->
			CTIntersection (t :: tl),punion (pos t) p2
		| _ ->
			CTIntersection ([t;t2,p2]),punion (pos t) p2
	in
	match%parser s with
	| [ (Arrow,pa) ] ->
		begin match%parser s with
		| [ [%let t2,p2 = parse_complex_type] ] -> make_fun t2 p2
		| [ ] ->
			if would_skip_display_position pa false s then begin
				let p = display_position#with_pos pa in
				make_fun (magic_type_ct p) p
			end else serror()
		end
	| [ (Binop OpAnd,pa) ] ->
		begin match%parser s with
		| [ [%let t2,p2 = parse_complex_type] ] -> make_intersection t2 p2
		| [ ] ->
			if would_skip_display_position pa false s then begin
				let p = display_position#with_pos pa in
				make_intersection (magic_type_ct p) p
			end else serror()
		end
	| [ ] -> t

and parse_function_type_next tl p1 = function%parser
	| [ (Arrow,pa); [%s s] ] ->
		begin match%parser s with
		| [ [%let tret = parse_complex_type_inner false] ] -> CTFunction (tl,tret), punion p1 (snd tret)
		| [ ] -> if would_skip_display_position pa false s then begin
				let ct = magic_type_th (display_position#with_pos pa) in
				CTFunction (tl,ct), punion p1 pa
			end else serror()
		end
	| [ ] -> serror ()

and parse_type_anonymous s =
	let p0 = popt question_mark s in
	match%parser s with
	| [ [%let name, p1 = dollar_ident]; parse_type_hint as t ] ->
		let opt,p1 = match p0 with
			| Some p -> true,punion p p1
			| None -> false,p1
		in
		let p2 = pos (last_token s) in
		let next acc =
			{
				cff_name = name,p1;
				cff_meta = if opt then [Meta.Optional,[],null_pos] else [];
				cff_access = [];
				cff_doc = None;
				cff_kind = FVar (Some t,None);
				cff_pos = punion p1 p2;
			} :: acc
		in
		begin match%parser s with
		| [ (BrClose,p2) ] -> next [],p2
		| [ (Comma,p2) ] ->
			(match%parser s with
			| [ (BrClose,p2) ] -> next [],p2
			| [ [%let l,p2 = parse_type_anonymous] ] -> next l,punion p1 p2
			| [ ] -> serror());
		| [ ] ->
			syntax_error (Expected [",";"}"]) s (next [],p2)
		end
	| [ ] ->
		if p0 = None then raise Stream.Failure else serror()

and parse_enum s =
	let doc = get_doc s in
	let meta = parse_meta s in
	match%parser s with
	| [ [%let name, p1 = ident]; parse_constraint_params as params ] ->
		let args = (match%parser s with
		| [ (POpen,_); [%let l = psep_trailing Comma parse_enum_param]; (PClose,_) ] -> l
		| [ ] -> []
		) in
		let t = popt parse_type_hint s in
		let p2 = (match%parser s with
			| [ semicolon as p ] -> p
			| [ ] -> serror()
		) in
		{
			ec_name = name,p1;
			ec_doc = doc_from_string_opt doc;
			ec_meta = meta;
			ec_args = args;
			ec_params = params;
			ec_type = t;
			ec_pos = punion p1 p2;
		}

and parse_enum_param = function%parser
	| [ (Question,_); [%let name,_p = ident]; parse_type_hint as t ] -> (name,true,t)
	| [ [%let name,_p = ident]; parse_type_hint as t ] -> (name,false,t)

and parse_function_field doc meta al = function%parser
	| [ (Kwd Function,p1); parse_fun_name as name; parse_constraint_params as pl; (POpen,_); [%let args = psep_trailing Comma parse_fun_param]; (PClose,_); [%let t = popt parse_type_hint]; [%s s] ] ->
		let e, p2 = (match%parser s with
			| [ expr as e ] ->
				ignore(semicolon s);
				Some e, pos e
			| [ semicolon as p ] -> None, p
			| [ ] -> serror()
		) in
		let f = {
			f_params = pl;
			f_args = args;
			f_type = t;
			f_expr = e;
		} in
		name,punion p1 p2,FFun f,al,meta

and parse_var_field_assignment = function%parser
	| [ (Binop OpAssign,_); [%s s] ] ->
		begin match%parser s with
		| [ (Binop OpLt,p1) ] ->
			let e = handle_xml_literal p1 in
			(* accept but don't expect semicolon *)
			let p2 = match%parser s with
				| [ (Semicolon,p) ] -> p
				| [ ] -> pos e
			in
			Some e,p2
		| [ expr as e; semicolon as p2 ] -> Some e , p2
		| [ ] -> serror()
		end
	| [ semicolon as p2 ] -> None , p2
	| [ ] -> serror()

and parse_class_field tdecl s =
	let doc = get_doc s in
	let meta = parse_meta s in
	match%parser s with
	| [ [%let al = plist parse_cf_rights] ] ->
		let check_optional opt name =
			if opt then begin
				if not tdecl then syntax_error (Custom "?var syntax is only allowed in structures") ~pos:(Some (pos name)) s ();
				(Meta.Optional,[],null_pos) :: meta
			end else
				meta
		in
		let name,pos,k,al,meta = (match%parser s with
		| [ (Kwd Var,p1); [%let opt,name = questionable_dollar_ident] ] ->
			let meta = check_optional opt name in
			begin match%parser s with
			| [ (POpen,_); property_ident as i1; (Comma,_); property_ident as i2; (PClose,_) ] ->
				let t = popt parse_type_hint s in
				let e,p2 = parse_var_field_assignment s in
				name,punion p1 p2,FProp (i1,i2,t,e),al,meta
			| [ [%let t = popt parse_type_hint] ] ->
				let e,p2 = parse_var_field_assignment s in
				name,punion p1 p2,FVar (t,e),al,meta
			end
		| [ (Kwd Final,p1) ] ->
			check_redundant_var p1 s;
			begin match%parser s with
			| [ [%let opt,name = questionable_dollar_ident]] ->
				begin match%parser s with
				| [(POpen,_); property_ident as i1; (Comma,_); property_ident as i2; (PClose,_); [%let t = popt parse_type_hint]; [%let e, p2 = parse_var_field_assignment] ] ->
					let meta = check_optional opt name in
					name,punion p1 p2,FProp(i1,i2,t,e),(al @ [AFinal,p1]),meta
				| [ [%let t = popt parse_type_hint]; [%let e,p2 = parse_var_field_assignment]] ->
					let meta = check_optional opt name in
					name,punion p1 p2,FVar(t,e),(al @ [AFinal,p1]),meta
				end
			| [ [%let al2 = plist parse_cf_rights]; [%let f = parse_function_field doc meta (al @ ((AFinal,p1) :: al2))] ] ->
				f
			| [ ] ->
				serror()
			end
		| [ [%let f = parse_function_field doc meta al] ] ->
			f
		| [ ] ->
			begin match List.rev al with
				| [] -> raise Stream.Failure
				| (AOverride,po) :: _ ->
					begin match check_completion po true s with
					| None ->
						serror()
					| Some(so,p) ->
						let f = {
							f_params = [];
							f_args = [];
							f_type = None;
							f_expr = None
						} in
						let name = match so with
							| None -> ""
							| Some s -> s
						in
						(name,p),punion po p,FFun f,al,(Meta.DisplayOverride,[],null_pos) :: meta
					end
				| _ -> serror()
			end
		) in
		let pos = match al with
			| [] -> pos
			| (_,p) :: _ -> punion p pos
		in
		{
			cff_name = name;
			cff_doc = doc_from_string_opt doc;
			cff_meta = meta;
			cff_access = al;
			cff_pos = pos;
			cff_kind = k;
		}

and parse_cf_rights = function%parser
	| [ (Kwd Static,p) ] -> AStatic,p
	| [ (Kwd Macro,p) ] -> AMacro,p
	| [ (Kwd Public,p) ] -> APublic,p
	| [ (Kwd Private,p) ] -> APrivate,p
	| [ (Kwd Override,p) ] -> AOverride,p
	| [ (Kwd Dynamic,p) ] -> ADynamic,p
	| [ (Kwd Inline,p) ] -> AInline,p
	| [ (Kwd Extern,p) ] -> AExtern,p
	| [ (Kwd Abstract,p) ] -> AAbstract,p
	| [ (Kwd Overload,p) ] -> AOverload,p

and parse_fun_name = function%parser
	| [ [%let i = dollar_ident] ] -> i
	| [ (Kwd New,p) ] -> "new",p

and parse_fun_param s =
	let meta = parse_meta s in
	match%parser s with
	| [ (Question,_); [%let name, pn = dollar_ident]; [%let t = popt parse_type_hint]; parse_fun_param_value as c ] -> ((name,pn),true,meta,t,c)
	| [ [%let name, pn = dollar_ident]; [%let t = popt parse_type_hint]; parse_fun_param_value as c ] -> ((name,pn),false,meta,t,c)
	| [ (Spread,_); [%let name, pn = dollar_ident]; [%let t = popt parse_type_hint]; parse_fun_param_value as c ] ->
		let t = match t with Some t -> t | None -> (ct_mono,null_pos) in
		let t = CTPath (make_ptp (mk_type_path ~params:[TPType t] (["haxe"],"Rest")) (snd t)),(snd t) in
		((name,pn),false,meta,Some t,c)

and parse_fun_param_value = function%parser
	| [ (Binop OpAssign,_); expr as e ] -> Some e
	| [ ] -> None

and parse_fun_param_type = function%parser
	| [ (Question,_); ident as name; parse_type_hint as t ] -> (name,true,t)
	| [ ident as name; parse_type_hint as t ] -> (name,false,t)

and parse_constraint_params = function%parser
	| [ (Binop OpLt,p); [%s s] ] ->
		begin match%parser s with
		| [ [%let l = psep_nonempty Comma parse_constraint_param]; (Binop OpGt,_) ] -> l
		| [ ] ->
			let pos = match%parser s with
			| [ (Binop OpGt,p) ] -> Some p (* junk > so we don't get weird follow-up errors *)
			| [ ] -> None
			in
			syntax_error (Expected ["type parameter"]) ~pos s [];
		end
	| [ ] -> []

and parse_constraint_param s =
	let meta = parse_meta s in
	match%parser s with
	| [ type_name as name ] ->
		let cto = (match%parser s with
			| [ (DblDot,_) ] ->
				(match%parser s with
				| [ parse_complex_type as t ] -> Some t
				| [ ] -> serror())
			| [ ] -> None
		) in
		let default = (match%parser s with
			| [ (Binop OpAssign,_) ] ->
				(match%parser s with
				| [ parse_complex_type as t ] -> Some t
				| [ ] -> serror())
			| [ ] -> None
		) in
		{
			tp_name = name;
			tp_params = [];
			tp_constraints = cto;
			tp_default = default;
			tp_meta = meta;
		}
	| [ ] ->
		(* If we have a metadata but no name afterwards, we have to fail hard. *)
		if meta <> [] then syntax_error (Expected ["type name"]) s ();
		raise Stream.Failure;

and parse_type_path_or_resume p1 s =
	let check_resume exc =
		if would_skip_display_position p1 true s then begin
			let p = display_position#with_pos p1 in
			make_ptp magic_type_path p,true
		end else
			raise exc
	in
	try
		let t = parse_type_path s in
		t,false
	with Stream.Failure | Stream.Error _ as exc -> check_resume exc

and parse_class_herit = function%parser
	| [ (Kwd Extends,p1); [%let t,_p = parse_type_path_or_resume p1] ] -> HExtends t
	| [ (Kwd Implements,p1); [%let t,_p = parse_type_path_or_resume p1] ] -> HImplements t

and block1 s = match%parser s with
	| [ [%let name, p = dollar_ident] ] -> block2 (name,p,NoQuotes) (Ident name) p s
	| [ (Const (String(name,qs)),p) ] -> block2 (name,p,DoubleQuotes) (String(name,qs)) p s (* STRINGTODO: qs... hmm *)
	| [ [%let b = block []] ] -> EBlock b

and block2 name ident p s =
	match%parser s with
	| [ (DblDot,_) ] ->
		let e = secure_expr s in
		fst (parse_obj_decl name e p s)
	| [ ] ->
		let f s =
			let e = expr_next (EConst ident,p) s in
			let _ = semicolon s in
			e
		in
		let el,_ = block_with_pos' [] f p s in
		EBlock el

and block acc s =
	fst (block_with_pos acc null_pos s)

and block_with_pos' acc f p s =
	try
		(* because of inner recursion, we can't put Display handling in errors below *)
		let e = f s in
		block_with_pos (e :: acc) (pos e) s
	with
		| Stream.Failure ->
			List.rev acc,p
		| Stream.Error msg when !in_display_file ->
			handle_stream_error msg s;
			(block_with_pos acc (next_pos s) s)

and block_with_pos acc p s =
	block_with_pos' acc parse_block_elt p s

and parse_block_var = function%parser
	| [ (Kwd Var,p1); [%let vl = parse_var_decls false p1]; semicolon as p2 ] ->
		(vl,punion p1 p2)
	| [ (Kwd Final,p1); [%s s] ] ->
		check_redundant_var p1 s;
		match%parser s with
		| [ [%let vl = parse_var_decls true p1]; semicolon as p2 ] ->
			(vl,punion p1 p2)
		| [ ] ->
			serror();

and parse_block_elt s = match%parser s with
	| [ [%let vl,p = parse_block_var] ] ->
		(EVars vl,p)
	| [ (Kwd Inline,p1) ] ->
		begin match%parser s with
		| [ (Kwd Function,_); [%let e = parse_function p1 true]; semicolon as _s ] -> e
		| [ secure_expr as e; semicolon as _s ] -> make_meta Meta.Inline [] e p1
		| [ ] -> serror()
		end
	| [ (Kwd Static,p) ] ->
		begin match%parser s with
		| [ [%let vl,p = parse_block_var] ] ->
			let vl = List.map (fun ev -> {ev with ev_static = true}) vl in
			(EVars vl,p)
		| [] -> syntax_error (Expected ["var";"final"]) s (mk_null_expr p)
		end
	| [ (Binop OpLt,p1) ] ->
		let e = handle_xml_literal p1 in
		(* accept but don't expect semicolon *)
		begin match%parser s with
			| [ (Semicolon,_) ] -> ()
			| [ ] -> ()
		end;
		e
	| [ expr as e; semicolon as _s ] -> e

and parse_obj_decl name e p0 s =
	let make_obj_decl el p1 =
		EObjectDecl (List.rev el),punion p0 p1
	in
	let rec loop p_end acc = match%parser s with
		| [ (Comma,p1) ] ->
			let next_expr key =
				let e = secure_expr s in
				loop (pos e) ((key,e) :: acc)
			in
			let next key = match%parser s with
				| [ (DblDot,_) ] ->
					next_expr key
				| [ ] ->
					syntax_error (Expected [":"]) s (next_expr key)
			in
			begin match%parser s with
				| [ [%let name, p = ident] ] -> next (name,p,NoQuotes)
				| [ (Const (String(name,qs)),p) ] -> next (name,p,DoubleQuotes) (* STRINGTODO: use qs? *)
				| [ ] -> acc,p_end
			end
		| [ ] -> acc,p_end
	in
	let el,p_end = loop p0 [name,e] in
	let e = make_obj_decl el p_end in
	e

and parse_array_decl p1 s =
	let resume_or_fail p1 =
		syntax_error (Expected ["expr";"]"]) s (
			let p = punion_next p1 s in
			[mk_null_expr p],p
		)
	in
	let el,p2 = match%parser s with
		| [ (BkClose,p2) ] -> [],p2
		| [ secure_expr as e0 ] ->
			let rec loop acc = match%parser s with
				| [ (Comma,pk) ] ->
					begin match%parser s with
						| [ (BkClose,p2) ] -> acc,p2
						| [ secure_expr as e ] -> loop (e :: acc)
						| [ ] ->
							syntax_error (Expected ["expr";"]"]) s (acc,pk)
					end
				| [ (BkClose,p2) ] -> acc,p2
				| [ ] ->
					syntax_error (Expected [",";"]"]) s (acc,next_pos s)
			in
			loop [e0]
		| [ ] -> resume_or_fail p1
	in
	EArrayDecl (List.rev el),punion p1 p2

and parse_var_decl_head final s =
	let meta = parse_meta s in
	match%parser s with
	| [ [%let name, p = dollar_ident] ] ->
		begin match%parser s with
		| [ [%let t = popt parse_type_hint] ] ->
			(meta,name,final,t,p)
		| [ (POpen,p1); property_ident as _p1; (Comma,_); property_ident as _p2; (PClose,p2); [%let t = popt parse_type_hint] ] ->
			syntax_error (Custom "Cannot define property accessors for local vars") ~pos:(Some (punion p1 p2)) s (meta,name,final,t,p)
		end
	| [ ] ->
		(* This nonsense is here for the var @ case in issue #9639 *)
		let rec loop meta = match meta with
			| (Meta.HxCompletion,_,p) :: _ -> (meta,"",false,None,null_pos)
			| _ :: meta -> loop meta
			| [] -> no_keyword "variable name" s
		in
		loop meta

and parse_var_assignment = function%parser
	| [ (Binop OpAssign,p1); [%s s] ] ->
		Some (secure_expr s)
	| [ ] -> None

and parse_var_assignment_resume final vl name pn t meta s =
	let eo = parse_var_assignment s in
	mk_evar ~final ?t ?eo ~meta (name,pn)

and parse_var_decls_next final vl = function%parser
	| [ (Comma,p1); [%let meta,name,final,t,pn = parse_var_decl_head final]; [%s s] ] ->
		let v_decl = parse_var_assignment_resume final vl name pn t meta s in
		parse_var_decls_next final (v_decl :: vl) s
	| [ ] ->
		vl

and parse_var_decls final p1 = function%parser
	| [ [%let meta,name,final,t,pn = parse_var_decl_head final]; [%s s] ] ->
		let v_decl = parse_var_assignment_resume final [] name pn t meta s in
		List.rev (parse_var_decls_next final [v_decl] s)
	| [ ] -> error (Custom "Missing variable identifier") p1

and parse_var_decl final = function%parser
	| [ [%let meta,name,final,t,pn = parse_var_decl_head final]; [%let v_decl = parse_var_assignment_resume final [] name pn t meta] ] -> v_decl

and inline_function = function%parser
	| [ (Kwd Inline,_); (Kwd Function,p1) ] -> true, p1
	| [ (Kwd Function,p1) ] -> false, p1

and parse_macro_expr p = function%parser
	| [ (DblDot,_); parse_complex_type as t ] ->
		let _, to_type, _  = reify !in_macro in
		let t = to_type t p in
		let ct = make_ptp_ct_null (mk_type_path ~sub:"ComplexType" (["haxe";"macro"],"Expr")) in
		(ECheckType (t,(ct,p)),p)
	| [ (Kwd Var,p1); [%let vl = psep Comma (parse_var_decl false)] ] ->
		reify_expr (EVars vl,p1) !in_macro
	| [ (Kwd Final,p1); [%s s] ] ->
		check_redundant_var p1 s;
		begin match%parser s with
		| [ [%let vl = psep Comma (parse_var_decl true)] ] ->
			reify_expr (EVars vl,p1) !in_macro
		| [ ] ->
			serror()
		end
	| [ [%let d = parse_class None [] [] false] ] ->
		let _,_,to_type = reify !in_macro in
		let ct = make_ptp_ct_null (mk_type_path ~sub:"TypeDefinition" (["haxe";"macro"],"Expr")) in
		(ECheckType (to_type d,(ct,null_pos)),p)
	| [ secure_expr as e ] ->
		reify_expr e !in_macro

and parse_function p1 inl s =
	let name = match%parser s with
		| [ dollar_ident as name ] -> Some name
		| [ ] -> None
	in
	let pl = parse_constraint_params s in
	match%parser s with
	| [ (POpen,_); [%let al = psep_trailing Comma parse_fun_param]; (PClose,_); [%let t = popt parse_type_hint] ] ->
		let make e =
			let f = {
				f_params = pl;
				f_type = t;
				f_args = al;
				f_expr = Some e;
			} in
			EFunction ((match name with None -> FKAnonymous | Some (name,pn) -> FKNamed ((name,pn),inl)),f), punion p1 (pos e)
		in
		make (secure_expr s)
	| [ ] ->
		(* Generate pseudo function to avoid toplevel-completion (issue #10691). We check against p1 here in order to cover cases
		   like `function a|b` *)
		if would_skip_display_position p1 false s then begin
			let null_func =
				let f = {
					f_params = [];
					f_type = None;
					f_args = [];
					f_expr = None
				} in
				let p = punion p1 (next_pos s) in
				let name = ("_hx_magic",p) in
				EFunction(FKNamed(name,inl),f),p
			in
			null_func
		end else
			serror()

and arrow_expr = function%parser
	| [ (Arrow,_); expr as e ] -> e
	| [ ] -> serror()

and arrow_function p1 al er s =
	let make e =
		let p = pos e in
		let return = (EMeta((Meta.ImplicitReturn, [], null_pos), (EReturn(Some e), p)), p) in
		EFunction(FKArrow, { f_params = []; f_type = None; f_args = al; f_expr = Some return;  }), punion p1 p
	in
	List.iter (fun (_,_,ml,_,_) ->	match ml with
		| (_,_,p) :: _ -> syntax_error (Custom "Metadata on arrow function arguments is not allowed") ~pos:(Some p) s ()
		| [] -> ()
	) al;
	make er

and arrow_ident_checktype e = (match e with
	| EConst(Ident n),p -> (n,p),None
	| ECheckType((EConst(Ident n),p),(t,pt)),_ -> (n,p),(Some (t,pt))
	| _ -> serror())

and arrow_first_param e s =
	(match fst e with
	| EConst(Ident ("true" | "false" | "null" | "this" | "super")) ->
		syntax_error (Custom "Invalid argument name") ~pos:(Some (pos e)) s (("",null_pos),false,[],None,None)
	| EConst(Ident n) ->
		(n,snd e),false,[],None,None
	| EBinop(OpAssign,e1,e2)
	| EParenthesis(EBinop(OpAssign,e1,e2),_) ->
		let (np,tpt) = arrow_ident_checktype e1 in np,true,[],tpt,(Some e2)
	| EParenthesis(e) ->
		let (np,tpt) = arrow_ident_checktype e in np,false,[],tpt,None
	| _ ->
		serror())

and expr s = match%parser s with
	| [ [%let name,params,p = parse_meta_entry] ] ->
		begin try
			make_meta name params (secure_expr s) p
		with
		| Stream.Failure | Stream.Error _ when !in_display_file ->
			let e = EConst (Ident "null"),null_pos in
			make_meta name params e p
		end
	| [ (Binop OpLt,p1) ] ->
		handle_xml_literal p1
	| [ (BrOpen,p1) ] ->
		(match%parser s with
		| [ block1 as b ] ->
			let p2 = match%parser s with
				| [ (BrClose,p2) ] -> p2
				| [ ] ->
					(* Ignore missing } if we are resuming and "guess" the last position. *)
					syntax_error (Expected ["}"]) s (pos (next_token s))
			in
			let e = (b,punion p1 p2) in
			(match b with
			| EObjectDecl _ -> expr_next e s
			| _ -> e)
		| [ ] ->
			check_resume p1 (fun() -> (EDisplay ((EObjectDecl [],p1),DKStructure),p1)) serror;
		)
	| [ (Kwd k,p) ] when !parsing_macro_cond ->
		expr_next (EConst (Ident (s_keyword k)), p) s
	| [ (Kwd Macro,p) ] ->
		begin match%parser s with
		| [ (Dot,pd); [%let e = parse_field (EConst (Ident "macro"),p) EFNormal pd] ] -> e
		| [ [%let e = parse_macro_expr p] ] -> e
		| [ ] -> serror()
		end
	| [ (Kwd Var,p1); [%let v = parse_var_decl false] ] -> (EVars [v],p1)
	| [ (Kwd Final,p1) ] ->
		check_redundant_var p1 s;
		begin match%parser s with
			| [ [%let v = parse_var_decl true] ] ->
				(EVars [v],p1)
			| [ ] ->
				serror()
		end
	| [ (Const c,p) ] -> expr_next (EConst c,p) s
	| [ (Kwd This,p) ] -> expr_next (EConst (Ident "this"),p) s
	| [ (Kwd Abstract,p) ] -> expr_next (EConst (Ident "abstract"),p) s
	| [ (Kwd True,p) ] -> expr_next (EConst (Ident "true"),p) s
	| [ (Kwd False,p) ] -> expr_next (EConst (Ident "false"),p) s
	| [ (Kwd Null,p) ] -> expr_next (EConst (Ident "null"),p) s
	| [ (Kwd Cast,p1) ] ->
		(match%parser s with
		| [ (POpen,pp); expr as e ] ->
			(match%parser s with
			| [ (Comma,pc); parse_complex_type as t; (PClose,p2) ] -> expr_next (ECast (e,Some t),punion p1 p2) s
			| [ [%let t,pt = parse_type_hint]; (PClose,p2) ] ->
				let ep = EParenthesis (ECheckType(e,(t,pt)),punion p1 p2), punion p1 p2 in
				expr_next (ECast (ep,None),punion p1 (pos ep)) s
			| [ (PClose,p2) ] ->
				let ep = expr_next (EParenthesis(e),punion pp p2) s in
				expr_next (ECast (ep,None),punion p1 (pos ep)) s
			| [ ] -> serror())
		| [ secure_expr as e ] -> expr_next (ECast (e,None),punion p1 (pos e)) s)
	| [ (Kwd Throw,p); expr as e ] -> (EThrow e,p)
	| [ (Kwd New,p1); [%let t,_p = parse_type_path_or_resume p1] ] ->
		begin match%parser s with
		| [ (POpen,po); [%let e = parse_call_params (fun el p2 -> (ENew(t,el)),punion p1 p2) po] ] -> expr_next e s
		| [ ] ->
			syntax_error (Expected ["("]) s (ENew(t,[]),punion p1 t.pos_full)
		end
	| [ (POpen,p1) ] -> (match%parser s with
		| [ (PClose,p2); arrow_expr as er; ] ->
			arrow_function p1 [] er s
		| [ (Question,p2); [%let al = psep_trailing Comma parse_fun_param]; (PClose,_); arrow_expr as er; ] ->
			let al = (match al with | (np,_,_,topt,e) :: al -> (np,true,[],topt,e) :: al | _ -> die "" __LOC__ ) in
			arrow_function p1 al er s
		| [  expr as e ] -> (match%parser s with
			| [ (PClose,p2) ] -> expr_next (EParenthesis e, punion p1 p2) s
			| [ (Comma,pc); [%let al = psep_trailing Comma parse_fun_param]; (PClose,_); arrow_expr as er; ] ->
				arrow_function p1 ((arrow_first_param e s) :: al) er s
			| [ [%let t,pt = parse_type_hint] ] -> (match%parser s with
				| [ (PClose,p2) ] -> expr_next (EParenthesis (ECheckType(e,(t,pt)),punion p1 p2), punion p1 p2) s
				| [ (Comma,pc); [%let al = psep_trailing Comma parse_fun_param]; (PClose,_); arrow_expr as er; ] ->
					let (np,_) = arrow_ident_checktype e in
					arrow_function p1 ((np,false,[],(Some(t,pt)),None) :: al) er s
				| [ ((Binop OpAssign),p2); expr as ea1 ] ->
					let with_args al er = (match fst e with
						| EConst(Ident n) ->
							arrow_function p1 (((n,snd e),true,[],(Some(t,pt)),(Some ea1)) :: al) er s
						| _ -> serror())
					in
					(match%parser s with
					| [ (PClose,p2); arrow_expr as er; ] ->
						with_args [] er
					| [ (Comma,pc); [%let al = psep_trailing Comma parse_fun_param]; (PClose,_); arrow_expr as er; ] ->
						with_args al er
					| [ ] -> serror())
				| [ ] -> serror())
			| [ ] ->
				syntax_error (Expected [")";",";":"]) s (expr_next (EParenthesis e, punion p1 (pos e)) s))
		)
	| [ (BkOpen,p1); [%let e = parse_array_decl p1] ] -> expr_next e s
	| [ (Kwd Function,p1); [%let e = parse_function p1 false]; ] -> e
	| [ (Unop op,p1); expr as e ] -> make_unop op e p1
	| [ (Spread,p1); expr as e ] -> make_unop Spread e (punion p1 (pos e))
	| [ (Binop OpSub,p1); expr as e ] ->
		make_unop Neg e p1
	(*/* removed unary + : this cause too much syntax errors go unnoticed, such as "a + + 1" (missing 'b')
						without adding anything to the language
	| [ (Binop OpAdd,p1) ] ->
		(match%parser s with
		| [ (Const (Int i),p); [%let e = expr_next (EConst (Int i),p)] ] -> e
		| [ (Const (Float f),p); [%let e = expr_next (EConst (Float f),p)] ] -> e
		| [ ] -> serror()) */*)
	| [ (Kwd For,p); (POpen,_); secure_expr as it ] ->
		let e = match%parser s with
			| [ (PClose,_); secure_expr as e ] -> e
			| [ ] ->
				syntax_error (Expected [")"]) s (mk_null_expr (pos it))
		in
		(EFor (it,e),punion p (pos e))
	| [ (Kwd If,p); (POpen,_); secure_expr as cond ] ->
		let e1 = match%parser s with
			| [ (PClose,_); secure_expr as e1 ] -> e1
			| [ ] ->
				syntax_error (Expected [")"]) s (mk_null_expr (pos cond))
		in
		let e2 = (match%parser s with
			| [ (Kwd Else,_); secure_expr as e2 ] -> Some e2
			| [ ] ->
				(* We check this in two steps to avoid the lexer missing tokens (#8565). *)
				match Stream.npeek 1 s with
				| [(Semicolon,_)] ->
					begin match Stream.npeek 2 s with
					| [(Semicolon,_);(Kwd Else,_)] ->
						Stream.junk s;
						Stream.junk s;
						Some (secure_expr s)
					| _ ->
						None
					end
				| _ ->
					None
		) in
		(EIf (cond,e1,e2), punion p (match e2 with None -> pos e1 | Some e -> pos e))
	| [ (Kwd Return,p) ] ->
		begin match%parser s with
		| [ expr as e ] -> (EReturn (Some e),punion p (pos e))
		| [ ] ->
			if would_skip_display_position p true s then (EReturn (Some (mk_null_expr (punion_next p s))),p)
			else (EReturn None,p)
		end
	| [ (Kwd Break,p) ] -> (EBreak,p)
	| [ (Kwd Continue,p) ] -> (EContinue,p)
	| [ (Kwd While,p1); (POpen,_); secure_expr as cond ] ->
		let e = match%parser s with
			| [ (PClose,_); secure_expr as e ] -> e
			| [ ] ->
				syntax_error (Expected [")"]) s (mk_null_expr (pos cond))
		in
		(EWhile (cond,e,NormalWhile),punion p1 (pos e))
	| [ (Kwd Do,p1); secure_expr as e ] ->
		begin match%parser s with
			| [ (Kwd While,_); (POpen,_); secure_expr as cond ] ->
				let p2 = expect_unless_resume_p PClose s in
				(EWhile (cond,e,DoWhile),punion p1 p2)
			| [ ] ->
				syntax_error (Expected ["while"]) s e (* ignore do *)
		end
	| [ (Kwd Switch,p1); secure_expr as e ] ->
		begin match%parser s with
			| [ (BrOpen,_); [%let cases, def = parse_switch_cases e []] ] ->
				let p2 = match%parser s with
					| [ (BrClose,p2) ] -> p2
					| [ ] ->
						(* Ignore missing } if we are resuming and "guess" the last position. *)
						syntax_error (Expected ["}"]) s (pos (next_token s))
				in
				(ESwitch (e,cases,def),punion p1 p2)
			| [ ] ->
				syntax_error (Expected ["{"]) s (ESwitch(e,[],None),punion p1 (pos e))
		end
	| [ (Kwd Try,p1); secure_expr as e; [%let cl,p2 = parse_catches e [] (pos e)] ] -> (ETry (e,cl),punion p1 p2)
	| [ (IntInterval i,p1); expr as e2 ] -> make_binop OpInterval (EConst (Int (i, None)),p1) e2
	| [ (Kwd Untyped,p1); secure_expr as e ] -> (EUntyped e,punion p1 (pos e))
	| [ (Dollar v,p) ] -> expr_next (EConst (Ident ("$"^v)),p) s
	| [ (Kwd Inline,p); secure_expr as e ] -> make_meta Meta.Inline [] e p

and expr_next e1 s =
	try
		expr_next' e1 s
	with Stream.Error msg when !in_display ->
		handle_stream_error msg s;
		e1

and expr_next' e1 s = match%parser s with
	| [ (BrOpen,p1); expr as eparam; (BrClose,p2) ] when is_dollar_ident e1 ->
		(match fst e1 with
		| EConst(Ident n) -> expr_next (EMeta((Meta.from_string n,[],snd e1),eparam), punion p1 p2) s
		| _ -> die "" __LOC__)
	| [ (Dot,p); [%let e = parse_field e1 EFNormal p] ] -> e
	| [ (QuestionDot,p); [%let e = parse_field e1 EFSafe p] ] -> e
	| [ (POpen,p1); [%let e = parse_call_params (fun el p2 -> (ECall(e1,el)),punion (pos e1) p2) p1] ] -> expr_next e s
	| [ (BkOpen,p1); secure_expr as e2 ] ->
		let p2 = expect_unless_resume_p BkClose s in
		let e2 = check_signature_mark e2 p1 p2 in
		expr_next (EArray (e1,e2), punion (pos e1) p2) s
	| [ (Arrow,pa) ] ->
		let er = secure_expr s in
		arrow_function (snd e1) [arrow_first_param e1 s] er s
	| [ (Binop OpGt,p1) ] ->
		(match%parser s with
		| [ (Binop OpGt,p2) ] when p1.pmax = p2.pmin ->
			(match%parser s with
			| [ (Binop OpGt,p3) ] when p2.pmax = p3.pmin ->
				(match%parser s with
				| [ (Binop OpAssign,p4); expr as e2 ] when p3.pmax = p4.pmin -> make_binop (OpAssignOp OpUShr) e1 e2
				| [ secure_expr as e2 ] -> make_binop OpUShr e1 e2)
			| [ (Binop OpAssign,p3); expr as e2 ] when p2.pmax = p3.pmin -> make_binop (OpAssignOp OpShr) e1 e2
			| [ secure_expr as e2 ] -> make_binop OpShr e1 e2)
		| [ (Binop OpAssign,p2) ] when p1.pmax = p2.pmin ->
			make_binop OpGte e1 (secure_expr s)
		| [ secure_expr as e2 ] ->
			make_binop OpGt e1 e2)
	| [ (Binop op,_); secure_expr as e2 ] -> make_binop op e1 e2
	| [ (Spread,_); secure_expr as e2 ] -> make_binop OpInterval e1 e2
	| [ (Unop op,p) ] when is_postfix e1 op ->
		expr_next (EUnop (op,Postfix,e1), punion (pos e1) p) s
	| [ (Question,_); expr as e2 ] ->
		begin match%parser s with
		| [ (DblDot,_); expr as e3 ] -> (ETernary (e1,e2,e3),punion (pos e1) (pos e3))
		| [ ] -> syntax_error (Expected [":"]) s e2
		end
	| [ (Kwd In,_); expr as e2 ] ->
		make_binop OpIn e1 e2
	| [ (Const (Ident "is"),p_is); parse_complex_type as t ] ->
		let p1 = pos e1 in
		let p2 = pos t in
		let e_is = EIs (e1,t), (punion p1 p2) in
		expr_next e_is s
	| [ ] -> e1

and parse_field e1 efk p s =
	check_resume p (fun () -> (EDisplay (e1,DKDot),p)) (fun () ->
		begin match%parser s with
		| [ (Kwd Macro,p2) ] when p.pmax = p2.pmin -> expr_next (EField (e1,"macro",efk) , punion (pos e1) p2) s
		| [ (Kwd Extern,p2) ] when p.pmax = p2.pmin -> expr_next (EField (e1,"extern",efk) , punion (pos e1) p2) s
		| [ (Kwd Function,p2) ] when p.pmax = p2.pmin -> expr_next (EField (e1,"function",efk) , punion (pos e1) p2) s
		| [ (Kwd New,p2) ] when p.pmax = p2.pmin -> expr_next (EField (e1,"new",efk) , punion (pos e1) p2) s
		| [ (Kwd k,p2) ] when !parsing_macro_cond && p.pmax = p2.pmin -> expr_next (EField (e1,s_keyword k,efk) , punion (pos e1) p2) s
		| [ (Const (Ident f),p2) ] when p.pmax = p2.pmin -> expr_next (EField (e1,f,efk) , punion (pos e1) p2) s
		| [ (Dollar v,p2) ] -> expr_next (EField (e1,"$"^v,efk) , punion (pos e1) p2) s
		| [ ] ->
			(* turn an integer followed by a dot into a float *)
			match e1 with
			| (EConst (Int (v, None)),p2) when p2.pmax = p.pmin -> expr_next (EConst (Float (v ^ ".", None)),punion p p2) s
			| _ -> serror()
		end
	)

and parse_guard = function%parser
	| [ (Kwd If,p1); (POpen,_); expr as e; (PClose,_); ] ->
		e

and expr_or_var = function%parser
	| [ (Kwd Var,p1); dollar_ident as np; ] -> EVars [mk_evar np],punion p1 (snd np)
	| [ (Kwd Final,p1); [%s s] ] ->
		check_redundant_var p1 s;
		begin match%parser s with
			| [ dollar_ident as np; ] ->
				EVars [mk_evar ~final:true np],punion p1 (snd np)
			| [ ] ->
				serror()
		end
	| [ secure_expr as e ] -> e

and parse_switch_cases eswitch cases s = match%parser s with
	| [ (Kwd Default,p1); (DblDot,pdot) ] ->
		let b,p2 = (block_with_pos [] p1 s) in
		let b = match b with
			| [] -> None,pdot
			| _ -> let p = punion p1 p2 in Some ((EBlock b,p)),p
		in
		let l , def = parse_switch_cases eswitch cases s in
		(match def with None -> () | Some _ -> syntax_error Duplicate_default ~pos:(Some p1) s ());
		l , Some b
	| [ (Kwd Case,p1); [%let el = psep Comma expr_or_var]; [%let eg = popt parse_guard] ] ->
		let pdot = expect_unless_resume_p DblDot s in
		if !was_auto_triggered then check_resume pdot (fun () -> ()) (fun () -> ());
		(match el with
		| [] -> syntax_error (Custom "case without a pattern is not allowed") ~pos:(Some p1) s ([],None)
		| _ ->
			let b,p2 = (block_with_pos [] p1 s) in
			let b,p = match b with
				| [] -> None,punion p1 pdot
				| _ -> let p = punion p1 p2 in Some ((EBlock b,p)),p
			in
			parse_switch_cases eswitch ((el,eg,b,p) :: cases) s
		)
	| [ ] ->
		List.rev cases , None

and parse_catch etry = function%parser
	| [ (Kwd Catch,p); (POpen,_); [%let name, pn = dollar_ident]; [%s s] ] ->
		match%parser s with
		| [ [%let t,pt = parse_type_hint]; (PClose,_); secure_expr as e ] -> ((name,pn),(Some (t,pt)),e,punion p (pos e)),(pos e)
		| [ (PClose,_); secure_expr as e ] -> ((name,pn),None,e,punion p (pos e)),(pos e)
		| [ (_,p) ] -> error Missing_type p

and parse_catches etry catches pmax = function%parser
	| [ [%let (catch,pmax) = parse_catch etry]; [%s s] ] -> parse_catches etry (catch :: catches) pmax s
	| [ ] -> List.rev catches,pmax

and parse_call_params f p1 s =
	if not !in_display_file then begin
		let el = psep_trailing Comma expr s in
		match%parser s with
		| [ (PClose,p2) ] -> f el p2
		| [ ] ->
			let expected = if el = [] then ["expression";")"] else [",";")"] in
			syntax_error (Expected expected) s (f el (last_pos s))
	end else begin
		let rec parse_next_param acc p1 =
			let e = try
				expr s
			with
			| Stream.Failure ->
				let expected = "expression" :: (if acc = [] then [")"] else []) in
				syntax_error (Expected expected) s ();
				mk_null_expr (punion_next p1 s)
			| Stream.Error msg ->
				handle_stream_error msg s;
				mk_null_expr (punion_next p1 s)
			in
			match%parser s with
			| [ (PClose,p2) ] ->
				let e = check_signature_mark e p1 p2 in
				f (List.rev (e :: acc)) p2
			| [ (Comma,p2)] ->
				begin match%parser s with
					| [ (PClose, p3) ] ->
						if (is_signature_display()) then begin
							let prev_arg_pos = punion p1 p2 in
							let comma_paren_pos = punion p2 p3 in
							(* first check wether the display position is within the previous argument *)
							if encloses_position_gt display_position#get prev_arg_pos then begin
								(* wrap the argument that was just parsed *)
								let e = mk_display_expr e DKMarked in
								f (List.rev (e :: acc)) p3
							(* then check wether the display position is between the comma and the closing parenthesis *)
							end else if encloses_position_gt display_position#get comma_paren_pos then begin
								(* add a dummy final argument *)
								let e2 = mk_display_expr (mk_null_expr comma_paren_pos) DKMarked in
								f (List.rev (e2 :: e :: acc)) p3
							end else f (List.rev (e :: acc)) p3
						end else begin
						(* if not in signature display mode don't check anything *)
							f (List.rev (e :: acc)) p3
						end
					| [] ->
						let e = check_signature_mark e p1 p2 in
						parse_next_param (e :: acc) p2
				end
			| [ ] ->
				let p2 = next_pos s in
				syntax_error (Expected [",";")"]) s ();
				let e = check_signature_mark e p1 p2 in
				f (List.rev (e :: acc)) p2
		in
		match%parser s with
		| [ (PClose,p2) ] -> f [] p2
		| [ ] -> parse_next_param [] p1
	end

(* Tries to parse a toplevel expression and defaults to a null expression when in display mode.
   This function always accepts in display mode and should only be used for expected expressions,
   not accepted ones! *)
and secure_expr = function%parser
	| [ expr as e ] -> e
	| [ [%s s] ] ->
		syntax_error (Expected ["expression"]) s (
			let last = last_token s in
			let plast = pos last in
			let offset = match fst last with
				| Const _ | Kwd _ | Dollar _ -> 1
				| _ -> 0
			in
			let plast = {plast with pmin = plast.pmax + offset} in
			mk_null_expr (punion_next plast s)
		)

let rec validate_macro_cond s e = match fst e with
	| EConst (Ident _)
	| EConst (String _)
	| EConst (Int (_, _))
	| EConst (Float (_, _))
		-> e
	| EUnop (op,p,e1) -> (EUnop (op, p, validate_macro_cond s e1), snd e)
	| EBinop (op,e1,e2) -> (EBinop(op, (validate_macro_cond s e1), (validate_macro_cond s e2)), snd e)
	| EParenthesis (e1) -> (EParenthesis (validate_macro_cond s e1), snd e)
	| EField(e1,name,efk) -> (EField(validate_macro_cond s e1,name,efk), snd e)
	| ECall ((EConst (Ident _),_) as i, args) -> (ECall (i,List.map (validate_macro_cond s) args),snd e)
	| _ -> syntax_error (Custom ("Invalid conditional expression")) ~pos:(Some (pos e)) s ((EConst (Ident "false"),(pos e)))

let parse_macro_ident t p s =
	if t = "display" then Hashtbl.replace special_identifier_files (Path.UniqueKey.create p.pfile) t;
	let e = (EConst (Ident t),p) in
	None, e

let rec parse_macro_cond s =
	parsing_macro_cond := true;
	try
		let cond = (match%parser s with
			| [ (Const (Ident t),p) ] ->
				parse_macro_ident t p s
			| [ (Const (String(s,qs)),p) ] ->
				None, (EConst (String(s,qs)),p)
			| [ (Const (Int (i, s)),p) ] ->
				None, (EConst (Int (i, s)),p)
			| [ (Const (Float (f, s)),p) ] ->
				None, (EConst (Float (f, s)),p)
			| [ (Kwd k,p) ] ->
				parse_macro_ident (s_keyword k) p s
			| [ (Unop op,p); [%let tk, e = parse_macro_cond] ] ->
				tk, make_unop op e p
			| [ (POpen,p1); [%let (e,p) = expr]; (PClose,p2) ] ->
				None, (EParenthesis(validate_macro_cond s (e,p)),punion p1 p2)) in
		parsing_macro_cond := false;
		cond
	with e ->
		parsing_macro_cond := false;
		raise e
