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

let popt f = parser
	| [< v = f >] -> Some v
	| [< >] -> None

let rec plist f = parser
	| [< v = f; l = plist f >] -> v :: l
	| [< >] -> []

let rec psep_nonempty sep f = parser
	| [< v = f; s >] ->
		let rec loop = parser
			| [< '(sep2,_) when sep2 = sep; v = f; l = loop >] -> v :: l
			| [< >] -> []
		in
		v :: loop s

let rec psep sep f = parser
	| [< r = psep_nonempty sep f >] -> r
	| [< >] -> []

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

let ident = parser
	| [< '(Const (Ident i),p) >] -> i,p

let dollar_ident = parser
	| [< '(Const (Ident i),p) >] -> i,p
	| [< '(Dollar i,p) >] -> ("$" ^ i),p

let dollar_ident_macro pack = parser
	| [< '(Const (Ident i),p) >] -> i,p
	| [< '(Dollar i,p) >] -> ("$" ^ i),p
	| [< '(Kwd Macro,p) when pack <> [] >] -> "macro", p
	| [< '(Kwd Extern,p) when pack <> [] >] -> "extern", p
	| [< '(Kwd Function,p) when pack <> [] >] -> "function", p

let lower_ident_or_macro = parser
	| [< '(Const (Ident i),p) when is_lower_ident i >] -> i
	| [< '(Kwd Macro,_) >] -> "macro"
	| [< '(Kwd Extern,_) >] -> "extern"
	| [< '(Kwd Function,_) >] -> "function"

let property_ident = parser
	| [< i,p = ident >] -> i,p
	| [< '(Kwd Dynamic,p) >] -> "dynamic",p
	| [< '(Kwd Default,p) >] -> "default",p
	| [< '(Kwd Null,p) >] -> "null",p

let questionable_dollar_ident s =
	let po = match s with parser
		| [< '(Question,p) >] -> Some p
		| [< >] -> None
	in
	let name,p = dollar_ident s in
	match po with
		| None ->
			false,(name,p)
		| Some p' ->
			if p.pmin <> p'.pmax then syntax_error (Custom (Printf.sprintf "Invalid usage of ?, use ?%s instead" name)) s ~pos:(Some p') ();
			true,(name,p)

let question_mark = parser
	| [< '(Question,p) >] -> p

let semicolon s =
	if fst (last_token s) = BrClose then
		match s with parser
		| [< '(Semicolon,p) >] -> p
		| [< >] -> snd (last_token s)
	else
		match s with parser
		| [< '(Semicolon,p) >] -> p
		| [< s >] ->
			syntax_error Missing_semicolon s (next_pos s)

let parsing_macro_cond = ref false

let rec	parse_file s =
	last_doc := None;
	match s with parser
	| [< '(Kwd Package,_); pack = parse_package; s >] ->
		begin match s with parser
		| [< '(Const(Ident _),p) when pack = [] >] -> error (Custom "Package name must start with a lowercase character") p
		| [< psem = semicolon; l = parse_type_decls TCAfterImport psem.pmax pack [] >] -> pack , l
		end
	| [< l = parse_type_decls TCBeforePackage (-1) [] [] >] -> [] , l

and parse_type_decls mode pmax pack acc s =
	check_type_decl_completion mode pmax s;
	let result = try
		begin match s with parser
		| [< cff = parse_type_decl mode >] -> Success cff
		| [< '(Eof,p) >] -> End p
		| [< >] -> Error ""
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

and parse_abstract doc meta flags p1 = parser
	| [< name = type_name; tl = parse_constraint_params; st = parse_abstract_subtype; sl = plist parse_abstract_relations; s >] ->
		let fl,p2 = match s with parser
			| [< '(BrOpen,_); fl, p2 = parse_class_fields false p1 >] -> fl,p2
			| [< >] -> syntax_error (Expected ["{";"to";"from"]) s ([],last_pos s)
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
			if not had_display && !in_display_file && display_position#enclosed_in p1 then
				syntax_completion (if List.mem HInterface n then SCInterfaceRelation else SCClassRelation) None (display_position#with_pos p1)
		in
		match s with parser
		| [< '(Kwd Extends,p1); t,b = parse_type_path_or_resume p1 >] ->
			check_display {p1 with pmin = p0.pmax; pmax = p1.pmin};
			let p0 = pos t in
			(* If we don't have type parameters, we have to offset by one so to not complete `extends`
				and `implements` after the identifier. *)
			let p0 = {p0 with pmax = p0.pmax + (if (fst t).tparams = [] then 1 else 0)} in
			loop (had_display || b) p0 ((HExtends t) :: acc)
		| [< '(Kwd Implements,p1); t,b = parse_type_path_or_resume p1 >] ->
			check_display {p1 with pmin = p0.pmax; pmax = p1.pmin};
			let p0 = pos t in
			let p0 = {p0 with pmax = p0.pmax + (if (fst t).tparams = [] then 1 else 0)} in
			loop (had_display || b) p0 ((HImplements t) :: acc)
		| [< '(BrOpen,p1) >] ->
			check_display {p1 with pmin = p0.pmax; pmax = p1.pmin};
			List.rev acc
		| [< >] ->
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
	match s with parser
	| [< '(Kwd Import,p1) >] -> parse_import s p1
	| [< '(Kwd Using,p1) >] -> parse_using s p1
	| [< doc = get_doc; meta = parse_meta; c = parse_common_flags; s >] ->
		match s with parser
		| [< '(Kwd Function,p1); name = dollar_ident; pl = parse_constraint_params; '(POpen,_); args = psep Comma parse_fun_param; '(PClose,_); t = popt parse_type_hint; s >] ->
			let e, p2 = (match s with parser
				| [< e = expr; s >] ->
					ignore(semicolon s);
					Some e, pos e
				| [< p = semicolon >] -> None, p
				| [< >] -> serror()
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
		| [< '(Kwd Var,p1); name = dollar_ident; s >] ->
			let p2,t =
				match s with parser
				| [< '(POpen,_); i1 = property_ident; '(Comma,_); i2 = property_ident; '(PClose,_) >] ->
					let t = popt parse_type_hint s in
					let e,p2 = parse_var_field_assignment s in
					p2,FProp (i1,i2,t,e)
				| [< t = popt parse_type_hint; s >] ->
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
		| [< '(Kwd Enum,p1) >] ->
			begin match s with parser
			| [< '(Kwd Abstract,p1); a,p = parse_abstract doc meta (AbEnum :: (convert_abstract_flags c)) p1 >] ->
				(EAbstract a,p)
			| [< name = type_name; tl = parse_constraint_params; '(BrOpen,_); l = plist parse_enum; '(BrClose,p2) >] ->
				(EEnum {
					d_name = name;
					d_doc = doc_from_string_opt doc;
					d_meta = meta;
					d_params = tl;
					d_flags = ExtList.List.filter_map decl_flag_to_enum_flag c;
					d_data = l
				}, punion p1 p2)
			end
		| [< n , p1 = parse_class_flags >] ->
			parse_class_content doc meta c n p1 s
		| [< '(Kwd Typedef,p1); name = type_name; tl = parse_constraint_params; '(Binop OpAssign,p2); t = parse_complex_type_at p2; s >] ->
			(match s with parser
			| [< '(Semicolon,_) >] -> ()
			| [< >] -> ());
			(ETypedef {
				d_name = name;
				d_doc = doc_from_string_opt doc;
				d_meta = meta;
				d_params = tl;
				d_flags = ExtList.List.filter_map decl_flag_to_enum_flag c;
				d_data = t;
			}, punion p1 (pos t))
		| [< '(Kwd Abstract,p1) >] ->
			begin match s with parser
			| [< a,p = parse_abstract doc meta (convert_abstract_flags c) p1 >] ->
				EAbstract a,p
			| [< >] ->
				let c2 = parse_common_flags s in
				begin match s with parser
				| [< flags,_ = parse_class_flags >] ->
					parse_class_content doc meta (c @ c2) (HAbstract :: flags) p1 s
				| [< >] ->
					serror()
				end
			end
		| [< >] ->
			match List.rev c with
			| (DFinal,p1) :: crest ->
				(match s with parser
				| [< name = dollar_ident; t = popt parse_type_hint; e,p2 = parse_var_field_assignment >] ->
					(EStatic {
						d_name = name;
						d_doc = doc_from_string_opt doc;
						d_meta = meta;
						d_params = [];
						d_flags = (ExtList.List.filter_map decl_flag_to_module_field_flag (List.rev crest)) @ [AFinal,p1];
						d_data = FVar(t,e);
					}, punion p1 p2)
				| [< >] -> check_type_decl_flag_completion mode c s)
			| _ ->
				check_type_decl_flag_completion mode c s


and parse_class doc meta cflags need_name s =
	let opt_name = if need_name then type_name else (fun s -> match popt type_name s with None -> "",null_pos | Some n -> n) in
	match s with parser
	| [< n , p1 = parse_class_flags; name = opt_name; tl = parse_constraint_params; hl = plist parse_class_herit; '(BrOpen,_); fl, p2 = parse_class_fields (not need_name) p1 >] ->
		(EClass {
			d_name = name;
			d_doc = doc;
			d_meta = meta;
			d_params = tl;
			d_flags = List.map fst cflags @ n @ hl;
			d_data = fl;
		}, punion p1 p2)

and parse_import s p1 =
	let rec loop pn acc =
		match s with parser
		| [< '(Dot,p) >] ->
			let resume() =
				type_path (List.map fst acc) true (punion pn p)
			in
			check_resume p resume (fun () -> ());
			begin match s with parser
			| [< '(Const (Ident k),p) >] ->
				loop pn ((k,p) :: acc)
			| [< '(Kwd Macro,p) >] ->
				loop pn (("macro",p) :: acc)
			| [< '(Kwd Extern,p) >] ->
				loop pn (("extern",p) :: acc)
			| [< '(Kwd Function,p) >] ->
				loop pn (("function",p) :: acc)
			| [< '(Binop OpMult,_); '(Semicolon,p2) >] ->
				p2, List.rev acc, IAll
			| [< >] ->
				ignore(popt semicolon s);
				syntax_error (Expected ["identifier"]) s (p,List.rev acc,INormal)
			end
		| [< '(Semicolon,p2) >] ->
			p2, List.rev acc, INormal
		| [< '(Kwd In,_); '(Const (Ident name),pname); '(Semicolon,p2) >] ->
			p2, List.rev acc, IAsName(name,pname)
		| [< '(Const (Ident "as"),_); '(Const (Ident name),pname); '(Semicolon,p2) >] ->
			p2, List.rev acc, IAsName(name,pname)
		| [< >] ->
			syntax_error (Expected [".";";";"as"]) s ((last_pos s),List.rev acc,INormal)
	in
	let p2, path, mode = (match s with parser
		| [< '(Const (Ident name),p) >] -> loop p [name,p]
		| [< >] ->
			if would_skip_display_position p1 true s then
				(display_position#with_pos p1,[],INormal)
			else
				syntax_error (Expected ["identifier"]) s (p1,[],INormal)
	) in
	(EImport (path,mode),punion p1 p2)

and parse_using s p1 =
	let rec loop pn acc =
		match s with parser
		| [< '(Dot,p) >] ->
			check_resume p (fun () -> type_path (List.map fst acc) false (punion pn p)) (fun () -> ());
			begin match s with parser
			| [< '(Const (Ident k),p) >] ->
				loop pn ((k,p) :: acc)
			| [< '(Kwd Macro,p) >] ->
				loop pn (("macro",p) :: acc)
			| [< '(Kwd Extern,p) >] ->
				loop pn (("extern",p) :: acc)
			| [< '(Kwd Function,p) >] ->
				loop pn (("function",p) :: acc)
			| [< >] ->
				syntax_error (Expected ["identifier"]) s (p,List.rev acc);
			end
		| [< '(Semicolon,p2) >] ->
			p2,List.rev acc
		| [< >] ->
			syntax_error (Expected [".";";"]) s ((last_pos s),List.rev acc)
	in
	let p2, path = (match s with parser
		| [< '(Const (Ident name),p) >] -> loop p [name,p]
		| [< >] ->
			if would_skip_display_position p1 true s then
				(display_position#with_pos p1,[])
			else
				syntax_error (Expected ["identifier"]) s (p1,[])
	) in
	(EUsing path,punion p1 p2)

and parse_abstract_relations s =
	let check_display p1 (ct,p2) =
		if !in_display_file && p1.pmax < (display_position#get).pmin && p2.pmin >= (display_position#get).pmax then
			(* This means we skipped the display position between the to/from and the type-hint we parsed.
			   Very weird case, it was probably a {} like in #7137. Let's discard it and use magic. *)
			((CTPath magic_type_path,display_position#with_pos p2))
		else
			(ct,p2)
	in
	match s with parser
	| [< '(Const (Ident "to"),p1); t = parse_complex_type_at p1 >] -> (AbTo (check_display p1 t))
	| [< '(Const (Ident "from"),p1); t = parse_complex_type_at p1 >] -> AbFrom (check_display p1 t)

and parse_abstract_subtype s =
	match s with parser
	| [< '(POpen, _); t = parse_complex_type; '(PClose,_) >] -> Some t
	| [< >] -> None

and parse_package s = psep Dot lower_ident_or_macro s

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
		begin match s with parser
		| [< cff = parse_class_field tdecl >] -> Success cff
		| [< '(BrClose,p) >] -> End p
		| [< >] -> Error ""
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
		let p2 = (match s with parser
			| [< '(BrClose,p2) >] -> p2
			| [< >] -> error (Expected ["}"]) (next_pos s)
		) in
		acc,p2
	end else
		parse_class_field_resume [] tdecl s

and parse_common_flags = parser
	| [< '(Kwd Private,p); l = parse_common_flags >] -> (DPrivate,p) :: l
	| [< '(Kwd Extern,p); l = parse_common_flags >] -> (DExtern,p) :: l
	| [< '(Kwd Final,p); l = parse_common_flags >] -> (DFinal,p) :: l
	| [< '(Kwd Macro,p); l = parse_common_flags >] -> (DMacro,p) :: l
	| [< '(Kwd Dynamic,p); l = parse_common_flags >] -> (DDynamic,p) :: l
	| [< '(Kwd Inline,p); l = parse_common_flags >] -> (DInline,p) :: l
	| [< '(Kwd Public,p); l = parse_common_flags >] -> (DPublic,p) :: l
	| [< '(Kwd Static,p); l = parse_common_flags >] -> (DStatic,p) :: l
	| [< '(Kwd Overload,p); l = parse_common_flags >] -> (DOverload,p) :: l
	| [< >] -> []

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

and parse_meta_params pname s = match s with parser
	| [< '(POpen,p) when p.pmin = pname.pmax; params = psep Comma parse_meta_argument_expr; >] ->
		ignore(expect_unless_resume_p PClose s);
		params
	| [< >] -> []

and parse_meta_entry = parser
	[< '(At,p1); s >] ->
		let meta = check_resume p1 (fun () -> Some (Meta.HxCompletion,[],p1)) (fun () -> None) in
		match s with parser
		| [< name,p = parse_meta_name p1; params = parse_meta_params p >] -> (name,params,punion p1 p)
		| [< >] -> match meta with None -> serror() | Some meta -> meta

and parse_meta = parser
	| [< entry = parse_meta_entry; s >] ->
		entry :: parse_meta s
	| [< >] -> []

and parse_meta_name_2 p1 acc s =
	let part,p = match s with parser
		| [< '(Const (Ident i),p) when p.pmin = p1.pmax >] -> i,p
		| [< '(Kwd k,p) when p.pmin = p1.pmax >] -> s_keyword k,p
	in
	let acc = part :: acc in
	match s with parser
	| [< '(Dot,p1); part,p2 = parse_meta_name_2 p1 acc >] -> part,punion p p2
	| [< >] -> acc,punion p1 p

and parse_meta_name p1 = parser
	| [< '(DblDot,p) when p.pmin = p1.pmax; s >] ->
		let meta = check_resume p (fun () -> Some (Meta.HxCompletion,p)) (fun() -> None) in
		begin match s with parser
		| [< name,p2 = parse_meta_name_2 p [] >] -> (Meta.parse (rev_concat "." name)),p2
		| [< >] -> match meta with None -> raise Stream.Failure | Some meta -> meta
		end
	| [< name,p2 = parse_meta_name_2 p1 [] >] -> (Meta.Custom (rev_concat "." name)),p2

and parse_enum_flags = parser
	| [< '(Kwd Enum,p) >] -> [] , p

and parse_class_flags = parser
	| [< '(Kwd Class,p) >] -> [] , p
	| [< '(Kwd Interface,p) >] -> [HInterface] , p

and parse_complex_type_at p = parser
	| [< t = parse_complex_type >] -> t
	| [< s >] ->
		if would_skip_display_position p false s then
			CTPath magic_type_path,display_position#with_pos p
		else
			serror()

and parse_type_hint = parser
	| [< '(DblDot,p1); s >] ->
		let f () = parse_complex_type_at p1 s in
		check_resume_range p1 s
			(fun p2 ->
				let ct = CTPath magic_type_path in
				pignore(f);
				ct,display_position#with_pos p1
			)
			f

and parse_type_opt = parser
	| [< t = parse_type_hint >] -> Some t
	| [< >] -> None

and parse_complex_type s = parse_complex_type_maybe_named false s

and parse_complex_type_maybe_named allow_named = parser
	| [< '(POpen,p1); tl = psep Comma (parse_complex_type_maybe_named true); '(PClose,p2); s >] ->
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
	| [< s >] ->
		let t = parse_complex_type_inner allow_named s in
		parse_complex_type_next t s

and parse_structural_extension = parser
	| [< '(Binop OpGt,p1); s >] ->
		match s with parser
		| [< t = parse_type_path >] ->
			begin match s with parser
				| [< '(Comma,_) >] -> t
				| [< >] -> syntax_error (Expected [","]) s t
			end;
		| [< >] ->
			if would_skip_display_position p1 false s then begin
				begin match s with parser
					| [< '(Comma,_) >] -> ()
					| [< >] -> ()
				end;
				magic_type_path,display_position#with_pos p1
			end else raise Stream.Failure

and parse_complex_type_inner allow_named = parser
	| [< '(POpen,p1); t = parse_complex_type; '(PClose,p2) >] -> CTParent t,punion p1 p2
	| [< '(BrOpen,p1); s >] ->
		(match s with parser
		| [< l,p2 = parse_type_anonymous >] -> CTAnonymous l,punion p1 p2
		| [< t = parse_structural_extension; s>] ->
			let tl = t :: plist parse_structural_extension s in
			(match s with parser
			| [< l,p2 = parse_type_anonymous >] -> CTExtend (tl,l),punion p1 p2
			| [< l,p2 = parse_class_fields true p1 >] -> CTExtend (tl,l),punion p1 p2)
		| [< l,p2 = parse_class_fields true p1 >] -> CTAnonymous l,punion p1 p2
		| [< >] -> serror())
	| [< '(Question,p1); t,p2 = parse_complex_type_inner allow_named >] ->
		CTOptional (t,p2),punion p1 p2
	| [< '(Spread,p1); t,p2 = parse_complex_type_inner allow_named >] ->
		let hint =
			match t with
			| CTNamed (_,hint) -> hint
			| _ -> (t,p2)
		in
		CTPath (mk_type_path ~params:[TPType hint] (["haxe"],"Rest")),punion p1 p2
	| [< n = dollar_ident; s >] ->
		(match s with parser
		| [< '(DblDot,_) when allow_named; t = parse_complex_type >] ->
			let p1 = snd n in
			let p2 = snd t in
			CTNamed (n,t),punion p1 p2
		| [< s >] ->
			let n,p = n in
			let t,p = parse_type_path2 None [] n p s in
			CTPath t,p)
	| [< t,p = parse_type_path >] ->
		CTPath t,p

and parse_type_path s = parse_type_path1 None [] s

and parse_type_path1 p0 pack = parser
	| [< name, p1 = dollar_ident_macro pack; s >] ->
		parse_type_path2 p0 pack name p1 s

and parse_type_path2 p0 pack name p1 s =
	let check_display f =
		let p = match p0 with
			| None -> p1
			| Some p -> punion p p1
		in
		if !in_display_file && display_position#enclosed_in p then begin
			mk_type_path (List.rev pack,name), p
		end else
			f()
	in
	if is_lower_ident name then
		(match s with parser
		| [< '(Dot,p) >] ->
			check_resume p
				(fun () -> raise (TypePath (List.rev (name :: pack),None,false,punion (match p0 with None -> p1 | Some p0 -> p0) p)))
				(fun () -> parse_type_path1 (match p0 with None -> Some p1 | Some _ -> p0) (name :: pack) s)
		| [< '(Semicolon,_) >] ->
			check_display (fun () -> error (Custom "Type name should start with an uppercase letter") p1)
		| [< >] ->
			check_display serror)
	else
		let sub,p2 = (match s with parser
			| [< '(Dot,p); s >] ->
				(check_resume p
					(fun () -> raise (TypePath (List.rev pack,Some (name,false),false,punion (match p0 with None -> p1 | Some p0 -> p0) p)))
					(fun () -> match s with parser
					| [< '(Const (Ident name),p2) when not (is_lower_ident name) >] -> Some name,p2
					| [< >] -> serror()))
			| [< >] -> None,p1
		) in
		let params,p2 = (match s with parser
			| [< '(Binop OpLt,plt); l = psep Comma (parse_type_path_or_const plt) >] ->
				begin match s with parser
				| [<'(Binop OpGt,p2) >] -> l,p2
				| [< >] ->
					syntax_error (Expected [">"]) s (l,pos (last_token s))
				end
			| [< >] -> [],p2
		) in
		let tp = mk_type_path ~params ?sub (List.rev pack,name)
		and pos = punion (match p0 with None -> p1 | Some p -> p) p2 in
		tp,pos

and type_name = parser
	| [< '(Const (Ident name),p); s >] ->
		if is_lower_ident name then
			syntax_error (Custom "Type name should start with an uppercase letter") ~pos:(Some p) s (name,p)
		else
			name,p
	| [< '(Dollar name,p) >] -> "$" ^ name,p

and parse_type_path_or_const plt = parser
	(* we can't allow (expr) here *)
	| [< '(BkOpen,p1); e = parse_array_decl p1 >] -> TPExpr (e)
	| [< t = parse_complex_type >] -> TPType t
	| [< '(Unop op,p1); '(Const c,p2) >] -> TPExpr (make_unop op (EConst c,p2) p1)
	| [< '(Binop OpSub,p1); '(Const c,p2) >] -> TPExpr (make_unop Neg (EConst c,p2) p1)
	| [< '(Const c,p) >] -> TPExpr (EConst c,p)
	| [< '(Kwd True,p) >] -> TPExpr (EConst (Ident "true"),p)
	| [< '(Kwd False,p) >] -> TPExpr (EConst (Ident "false"),p)
	| [< e = expr >] -> TPExpr e
	| [< s >] ->
		if !in_display_file then begin
			if would_skip_display_position plt false s then begin
				let ct = CTPath magic_type_path in
				TPType (ct,display_position#with_pos plt)
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
	match s with parser
	| [< '(Arrow,pa); s >] ->
		begin match s with parser
		| [< t2,p2 = parse_complex_type >] -> make_fun t2 p2
		| [< >] ->
			if would_skip_display_position pa false s then begin
				let ct = CTPath magic_type_path in
				make_fun ct (display_position#with_pos pa)
			end else serror()
		end
	| [< '(Binop OpAnd,pa); s >] ->
		begin match s with parser
		| [< t2,p2 = parse_complex_type >] -> make_intersection t2 p2
		| [< >] ->
			if would_skip_display_position pa false s then begin
				let ct = CTPath magic_type_path in
				make_intersection ct (display_position#with_pos pa)
			end else serror()
		end
	| [< >] -> t

and parse_function_type_next tl p1 = parser
	| [< '(Arrow,pa); s >] ->
		begin match s with parser
		| [< tret = parse_complex_type_inner false >] -> CTFunction (tl,tret), punion p1 (snd tret)
		| [< >] -> if would_skip_display_position pa false s then begin
				let ct = (CTPath magic_type_path),(display_position#with_pos pa) in
				CTFunction (tl,ct), punion p1 pa
			end else serror()
		end
	| [< >] -> serror ()

and parse_type_anonymous s =
	let p0 = popt question_mark s in
	match s with parser
	| [< name, p1 = dollar_ident; t = parse_type_hint; s >] ->
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
		begin match s with parser
		| [< '(BrClose,p2) >] -> next [],p2
		| [< '(Comma,p2) >] ->
			(match s with parser
			| [< '(BrClose,p2) >] -> next [],p2
			| [< l,p2 = parse_type_anonymous >] -> next l,punion p1 p2
			| [< >] -> serror());
		| [< >] ->
			syntax_error (Expected [",";"}"]) s (next [],p2)
		end
	| [< >] ->
		if p0 = None then raise Stream.Failure else serror()

and parse_enum s =
	let doc = get_doc s in
	let meta = parse_meta s in
	match s with parser
	| [< name, p1 = ident; params = parse_constraint_params; s >] ->
		let args = (match s with parser
		| [< '(POpen,_); l = psep Comma parse_enum_param; '(PClose,_) >] -> l
		| [< >] -> []
		) in
		let t = popt parse_type_hint s in
		let p2 = (match s with parser
			| [< p = semicolon >] -> p
			| [< >] -> serror()
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

and parse_enum_param = parser
	| [< '(Question,_); name, _ = ident; t = parse_type_hint >] -> (name,true,t)
	| [< name, _ = ident; t = parse_type_hint >] -> (name,false,t)

and parse_function_field doc meta al = parser
	| [< '(Kwd Function,p1); name = parse_fun_name; pl = parse_constraint_params; '(POpen,_); args = psep Comma parse_fun_param; '(PClose,_); t = popt parse_type_hint; s >] ->
		let e, p2 = (match s with parser
			| [< e = expr; s >] ->
				ignore(semicolon s);
				Some e, pos e
			| [< p = semicolon >] -> None, p
			| [< >] -> serror()
		) in
		let f = {
			f_params = pl;
			f_args = args;
			f_type = t;
			f_expr = e;
		} in
		name,punion p1 p2,FFun f,al,meta

and parse_var_field_assignment = parser
	| [< '(Binop OpAssign,_); s >] ->
		begin match s with parser
		| [< '(Binop OpLt,p1); s >] ->
			let e = handle_xml_literal p1 in
			(* accept but don't expect semicolon *)
			let p2 = match s with parser
				| [< '(Semicolon,p) >] -> p
				| [< >] -> pos e
			in
			Some e,p2
		| [< e = expr; p2 = semicolon >] -> Some e , p2
		| [< >] -> serror()
		end
	| [< p2 = semicolon >] -> None , p2
	| [< >] -> serror()

and parse_class_field tdecl s =
	let doc = get_doc s in
	let meta = parse_meta s in
	match s with parser
	| [< al = plist parse_cf_rights; s >] ->
		let check_optional opt name =
			if opt then begin
				if not tdecl then syntax_error (Custom "?var syntax is only allowed in structures") ~pos:(Some (pos name)) s ();
				(Meta.Optional,[],null_pos) :: meta
			end else
				meta
		in
		let name,pos,k,al,meta = (match s with parser
		| [< '(Kwd Var,p1); opt,name = questionable_dollar_ident; s >] ->
			let meta = check_optional opt name in
			begin match s with parser
			| [< '(POpen,_); i1 = property_ident; '(Comma,_); i2 = property_ident; '(PClose,_) >] ->
				let t = popt parse_type_hint s in
				let e,p2 = parse_var_field_assignment s in
				name,punion p1 p2,FProp (i1,i2,t,e),al,meta
			| [< t = popt parse_type_hint; s >] ->
				let e,p2 = parse_var_field_assignment s in
				name,punion p1 p2,FVar (t,e),al,meta
			end
		| [< '(Kwd Final,p1) >] ->
			begin match s with parser
			| [< opt,name = questionable_dollar_ident; t = popt parse_type_hint; e,p2 = parse_var_field_assignment >] ->
				let meta = check_optional opt name in
				name,punion p1 p2,FVar(t,e),(al @ [AFinal,p1]),meta
			| [< al2 = plist parse_cf_rights; f = parse_function_field doc meta (al @ ((AFinal,p1) :: al2)) >] ->
				f
			| [< >] ->
				serror()
			end
		| [< f = parse_function_field doc meta al >] ->
			f
		| [< >] ->
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

and parse_cf_rights = parser
	| [< '(Kwd Static,p) >] -> AStatic,p
	| [< '(Kwd Macro,p) >] -> AMacro,p
	| [< '(Kwd Public,p) >] -> APublic,p
	| [< '(Kwd Private,p) >] -> APrivate,p
	| [< '(Kwd Override,p) >] -> AOverride,p
	| [< '(Kwd Dynamic,p) >] -> ADynamic,p
	| [< '(Kwd Inline,p) >] -> AInline,p
	| [< '(Kwd Extern,p) >] -> AExtern,p
	| [< '(Kwd Abstract,p) >] -> AAbstract,p
	| [< '(Kwd Overload,p) >] -> AOverload,p

and parse_fun_name = parser
	| [< name,p = dollar_ident >] -> name,p
	| [< '(Kwd New,p) >] -> "new",p

and parse_fun_param s =
	let meta = parse_meta s in
	match s with parser
	| [< '(Question,_); name, pn = dollar_ident; t = popt parse_type_hint; c = parse_fun_param_value >] -> ((name,pn),true,meta,t,c)
	| [< name, pn = dollar_ident; t = popt parse_type_hint; c = parse_fun_param_value >] -> ((name,pn),false,meta,t,c)
	| [< '(Spread,_); name, pn = dollar_ident; t = popt parse_type_hint; c = parse_fun_param_value >] ->
		let t = match t with Some t -> t | None -> (ct_mono,null_pos) in
		let t = CTPath (mk_type_path ~params:[TPType t] (["haxe"],"Rest")), snd t in
		((name,pn),false,meta,Some t,c)

and parse_fun_param_value = parser
	| [< '(Binop OpAssign,_); e = expr >] -> Some e
	| [< >] -> None

and parse_fun_param_type = parser
	| [< '(Question,_); name = ident; t = parse_type_hint >] -> (name,true,t)
	| [< name = ident; t = parse_type_hint >] -> (name,false,t)

and parse_constraint_params = parser
	| [< '(Binop OpLt,p); s >] ->
		begin match s with parser
		| [< l = psep_nonempty Comma parse_constraint_param; '(Binop OpGt,_) >] -> l
		| [< >] ->
			let pos = match s with parser
			| [< '(Binop OpGt,p) >] -> Some p (* junk > so we don't get weird follow-up errors *)
			| [< >] -> None
			in
			syntax_error (Expected ["type parameter"]) ~pos s [];
		end
	| [< >] -> []

and parse_constraint_param s =
	let meta = parse_meta s in
	match s with parser
	| [< name = type_name; s >] ->
		let cto = (match s with parser
			| [< '(DblDot,_); s >] ->
				(match s with parser
				| [< t = parse_complex_type >] -> Some t
				| [< >] -> serror())
			| [< >] -> None
		) in
		let default = (match s with parser
			| [< '(Binop OpAssign,_); s >] ->
				(match s with parser
				| [< t = parse_complex_type >] -> Some t
				| [< >] -> serror())
			| [< >] -> None
		) in
		{
			tp_name = name;
			tp_params = [];
			tp_constraints = cto;
			tp_default = default;
			tp_meta = meta;
		}
	| [< >] ->
		(* If we have a metadata but no name afterwards, we have to fail hard. *)
		if meta <> [] then syntax_error (Expected ["type name"]) s ();
		raise Stream.Failure;

and parse_type_path_or_resume p1 s =
	let check_resume exc =
		if would_skip_display_position p1 true s then
			(magic_type_path,display_position#with_pos p1),true
		else
			raise exc
	in
	try
		let t = parse_type_path s in
		t,false
	with Stream.Failure | Stream.Error _ as exc -> check_resume exc

and parse_class_herit = parser
	| [< '(Kwd Extends,p1); t,_ = parse_type_path_or_resume p1 >] -> HExtends t
	| [< '(Kwd Implements,p1); t,_ = parse_type_path_or_resume p1 >] -> HImplements t

and block1 = parser
	| [< name,p = dollar_ident; s >] -> block2 (name,p,NoQuotes) (Ident name) p s
	| [< '(Const (String(name,qs)),p); s >] -> block2 (name,p,DoubleQuotes) (String(name,qs)) p s (* STRINGTODO: qs... hmm *)
	| [< b = block [] >] -> EBlock b

and block2 name ident p s =
	match s with parser
	| [< '(DblDot,_) >] ->
		let e = secure_expr s in
		fst (parse_obj_decl name e p s)
	| [< >] ->
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
		| Error (e,p) when !in_display_file ->
			block_with_pos acc p s

and block_with_pos acc p s =
	block_with_pos' acc parse_block_elt p s

and parse_block_var = parser
	| [< '(Kwd Var,p1); vl = parse_var_decls false p1; p2 = semicolon >] ->
		(vl,punion p1 p2)
	| [< '(Kwd Final,p1); vl = parse_var_decls true p1; p2 = semicolon >] ->
		(vl,punion p1 p2)

and parse_block_elt = parser
	| [< (vl,p) = parse_block_var >] ->
		(EVars vl,p)
	| [< '(Kwd Inline,p1); s >] ->
		begin match s with parser
		| [< '(Kwd Function,_); e = parse_function p1 true; _ = semicolon >] -> e
		| [< e = secure_expr; _ = semicolon >] -> make_meta Meta.Inline [] e p1
		| [< >] -> serror()
		end
	| [< '(Kwd Static,p); s >] ->
		begin match s with parser
		| [< (vl,p) = parse_block_var >] ->
			let vl = List.map (fun ev -> {ev with ev_static = true}) vl in
			(EVars vl,p)
		| [<>] -> syntax_error (Expected ["var";"final"]) s (mk_null_expr p)
		end
	| [< '(Binop OpLt,p1); s >] ->
		let e = handle_xml_literal p1 in
		(* accept but don't expect semicolon *)
		begin match s with parser
			| [< '(Semicolon,_) >] -> ()
			| [< >] -> ()
		end;
		e
	| [< e = expr; _ = semicolon >] -> e

and parse_obj_decl name e p0 s =
	let make_obj_decl el p1 =
		EObjectDecl (List.rev el),punion p0 p1
	in
	let rec loop p_end acc = match s with parser
		| [< '(Comma,p1); s >] ->
			let next_expr key =
				let e = secure_expr s in
				loop (pos e) ((key,e) :: acc)
			in
			let next key = match s with parser
				| [< '(DblDot,_) >] ->
					next_expr key
				| [< >] ->
					syntax_error (Expected [":"]) s (next_expr key)
			in
			begin match s with parser
				| [< name,p = ident >] -> next (name,p,NoQuotes)
				| [< '(Const (String(name,qs)),p) >] -> next (name,p,DoubleQuotes) (* STRINGTODO: use qs? *)
				| [< >] -> acc,p_end
			end
		| [< >] -> acc,p_end
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
	let el,p2 = match s with parser
		| [< '(BkClose,p2) >] -> [],p2
		| [< e0 = secure_expr >] ->
			let rec loop acc = match s with parser
				| [< '(Comma,pk) >] ->
					begin match s with parser
						| [< '(BkClose,p2) >] -> acc,p2
						| [< e = secure_expr >] -> loop (e :: acc)
						| [< >] ->
							syntax_error (Expected ["expr";"]"]) s (acc,pk)
					end
				| [< '(BkClose,p2) >] -> acc,p2
				| [< >] ->
					syntax_error (Expected [",";"]"]) s (acc,next_pos s)
			in
			loop [e0]
		| [< >] -> resume_or_fail p1
	in
	EArrayDecl (List.rev el),punion p1 p2

and parse_var_decl_head final s =
	let meta = parse_meta s in
	match s with parser
	| [< name, p = dollar_ident; t = popt parse_type_hint >] -> (meta,name,final,t,p)
	| [< >] ->
		(* This nonsense is here for the var @ case in issue #9639 *)
		let rec loop meta = match meta with
			| (Meta.HxCompletion,_,p) :: _ -> (meta,"",false,None,null_pos)
			| _ :: meta -> loop meta
			| [] -> no_keyword "variable name" s
		in
		loop meta

and parse_var_assignment = parser
	| [< '(Binop OpAssign,p1); s >] ->
		Some (secure_expr s)
	| [< >] -> None

and parse_var_assignment_resume final vl name pn t meta s =
	let eo = parse_var_assignment s in
	mk_evar ~final ?t ?eo ~meta (name,pn)

and parse_var_decls_next final vl = parser
	| [< '(Comma,p1); meta,name,final,t,pn = parse_var_decl_head final; s >] ->
		let v_decl = parse_var_assignment_resume final vl name pn t meta s in
		parse_var_decls_next final (v_decl :: vl) s
	| [< >] ->
		vl

and parse_var_decls final p1 = parser
	| [< meta,name,final,t,pn = parse_var_decl_head final; s >] ->
		let v_decl = parse_var_assignment_resume final [] name pn t meta s in
		List.rev (parse_var_decls_next final [v_decl] s)
	| [< >] -> error (Custom "Missing variable identifier") p1

and parse_var_decl final = parser
	| [< meta,name,final,t,pn = parse_var_decl_head final; v_decl = parse_var_assignment_resume final [] name pn t meta >] -> v_decl

and inline_function = parser
	| [< '(Kwd Inline,_); '(Kwd Function,p1) >] -> true, p1
	| [< '(Kwd Function,p1) >] -> false, p1

and parse_macro_expr p = parser
	| [< '(DblDot,_); t = parse_complex_type >] ->
		let _, to_type, _  = reify !in_macro in
		let t = to_type t p in
		(ECheckType (t,(CTPath (mk_type_path ~sub:"ComplexType" (["haxe";"macro"],"Expr")),null_pos)),p)
	| [< '(Kwd Var,p1); vl = psep Comma (parse_var_decl false) >] ->
		reify_expr (EVars vl,p1) !in_macro
	| [< '(Kwd Final,p1); vl = psep Comma (parse_var_decl true) >] ->
		reify_expr (EVars vl,p1) !in_macro
	| [< d = parse_class None [] [] false >] ->
		let _,_,to_type = reify !in_macro in
		(ECheckType (to_type d,(CTPath (mk_type_path ~sub:"TypeDefinition" (["haxe";"macro"],"Expr")),null_pos)),p)
	| [< e = secure_expr >] ->
		reify_expr e !in_macro

and parse_function p1 inl = parser
	| [< name = popt dollar_ident; pl = parse_constraint_params; '(POpen,_); al = psep Comma parse_fun_param; '(PClose,_); t = popt parse_type_hint; s >] ->
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

and arrow_expr = parser
	| [< '(Arrow,_); e = expr >] -> e
	| [< >] -> serror()

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

and expr = parser
	| [< (name,params,p) = parse_meta_entry; s >] ->
		begin try
			make_meta name params (secure_expr s) p
		with
		| Stream.Failure | Stream.Error _ when !in_display_file ->
			let e = EConst (Ident "null"),null_pos in
			make_meta name params e p
		end
	| [< '(Binop OpLt,p1) >] ->
		handle_xml_literal p1
	| [< '(BrOpen,p1); s >] ->
		(match s with parser
		| [< b = block1; s >] ->
			let p2 = match s with parser
				| [< '(BrClose,p2) >] -> p2
				| [< >] ->
					(* Ignore missing } if we are resuming and "guess" the last position. *)
					syntax_error (Expected ["}"]) s (pos (next_token s))
			in
			let e = (b,punion p1 p2) in
			(match b with
			| EObjectDecl _ -> expr_next e s
			| _ -> e)
		| [< >] ->
			check_resume p1 (fun() -> (EDisplay ((EObjectDecl [],p1),DKStructure),p1)) serror;
		)
	| [< '(Kwd k,p) when !parsing_macro_cond; s >] ->
		expr_next (EConst (Ident (s_keyword k)), p) s
	| [< '(Kwd Macro,p); s >] ->
		begin match s with parser
		| [< '(Dot,pd); e = parse_field (EConst (Ident "macro"),p) pd >] -> e
		| [< e = parse_macro_expr p >] -> e
		| [< >] -> serror()
		end
	| [< '(Kwd Var,p1); v = parse_var_decl false >] -> (EVars [v],p1)
	| [< '(Kwd Final,p1); v = parse_var_decl true >] -> (EVars [v],p1)
	| [< '(Const c,p); s >] -> expr_next (EConst c,p) s
	| [< '(Kwd This,p); s >] -> expr_next (EConst (Ident "this"),p) s
	| [< '(Kwd Abstract,p); s >] -> expr_next (EConst (Ident "abstract"),p) s
	| [< '(Kwd True,p); s >] -> expr_next (EConst (Ident "true"),p) s
	| [< '(Kwd False,p); s >] -> expr_next (EConst (Ident "false"),p) s
	| [< '(Kwd Null,p); s >] -> expr_next (EConst (Ident "null"),p) s
	| [< '(Kwd Cast,p1); s >] ->
		(match s with parser
		| [< '(POpen,pp); e = expr; s >] ->
			(match s with parser
			| [< '(Comma,pc); t = parse_complex_type; '(PClose,p2); s >] -> expr_next (ECast (e,Some t),punion p1 p2) s
			| [< t,pt = parse_type_hint; '(PClose,p2); s >] ->
				let ep = EParenthesis (ECheckType(e,(t,pt)),punion p1 p2), punion p1 p2 in
				expr_next (ECast (ep,None),punion p1 (pos ep)) s
			| [< '(PClose,p2); s >] ->
				let ep = expr_next (EParenthesis(e),punion pp p2) s in
				expr_next (ECast (ep,None),punion p1 (pos ep)) s
			| [< >] -> serror())
		| [< e = secure_expr >] -> expr_next (ECast (e,None),punion p1 (pos e)) s)
	| [< '(Kwd Throw,p); e = expr >] -> (EThrow e,p)
	| [< '(Kwd New,p1); t,_ = parse_type_path_or_resume p1; s >] ->
		begin match s with parser
		| [< '(POpen,po); e = parse_call_params (fun el p2 -> (ENew(t,el)),punion p1 p2) po >] -> expr_next e s
		| [< >] ->
			syntax_error (Expected ["("]) s (ENew(t,[]),punion p1 (pos t))
		end
	| [< '(POpen,p1); s >] -> (match s with parser
		| [< '(PClose,p2); er = arrow_expr; >] ->
			arrow_function p1 [] er s
		| [< '(Question,p2); al = psep Comma parse_fun_param; '(PClose,_); er = arrow_expr; >] ->
			let al = (match al with | (np,_,_,topt,e) :: al -> (np,true,[],topt,e) :: al | _ -> die "" __LOC__ ) in
			arrow_function p1 al er s
		| [<  e = expr; s >] -> (match s with parser
			| [< '(PClose,p2); s >] -> expr_next (EParenthesis e, punion p1 p2) s
			| [< '(Comma,pc); al = psep Comma parse_fun_param; '(PClose,_); er = arrow_expr; >] ->
				arrow_function p1 ((arrow_first_param e s) :: al) er s
			| [< t,pt = parse_type_hint; s >] -> (match s with parser
				| [< '(PClose,p2); s >] -> expr_next (EParenthesis (ECheckType(e,(t,pt)),punion p1 p2), punion p1 p2) s
				| [< '(Comma,pc); al = psep Comma parse_fun_param; '(PClose,_); er = arrow_expr; >] ->
					let (np,_) = arrow_ident_checktype e in
					arrow_function p1 ((np,false,[],(Some(t,pt)),None) :: al) er s
				| [< '((Binop OpAssign),p2); ea1 = expr; s >] ->
					let with_args al er = (match fst e with
						| EConst(Ident n) ->
							arrow_function p1 (((n,snd e),true,[],(Some(t,pt)),(Some ea1)) :: al) er s
						| _ -> serror())
					in
					(match s with parser
					| [< '(PClose,p2); er = arrow_expr; >] ->
						with_args [] er
					| [< '(Comma,pc); al = psep Comma parse_fun_param; '(PClose,_); er = arrow_expr; >] ->
						with_args al er
					| [< >] -> serror())
				| [< >] -> serror())
			| [< >] ->
				syntax_error (Expected [")";",";":"]) s (expr_next (EParenthesis e, punion p1 (pos e)) s))
		)
	| [< '(BkOpen,p1); e = parse_array_decl p1; s >] -> expr_next e s
	| [< '(Kwd Function,p1); e = parse_function p1 false; >] -> e
	| [< '(Unop op,p1); e = expr >] -> make_unop op e p1
	| [< '(Spread,p1); e = expr >] -> make_unop Spread e (punion p1 (pos e))
	| [< '(Binop OpSub,p1); e = expr >] ->
		make_unop Neg e p1
	(*/* removed unary + : this cause too much syntax errors go unnoticed, such as "a + + 1" (missing 'b')
						without adding anything to the language
	| [< '(Binop OpAdd,p1); s >] ->
		(match s with parser
		| [< '(Const (Int i),p); e = expr_next (EConst (Int i),p) >] -> e
		| [< '(Const (Float f),p); e = expr_next (EConst (Float f),p) >] -> e
		| [< >] -> serror()) */*)
	| [< '(Kwd For,p); '(POpen,_); it = secure_expr; s >] ->
		let e = match s with parser
			| [< '(PClose,_); e = secure_expr >] -> e
			| [< >] ->
				syntax_error (Expected [")"]) s (mk_null_expr (pos it))
		in
		(EFor (it,e),punion p (pos e))
	| [< '(Kwd If,p); '(POpen,_); cond = secure_expr; s >] ->
		let e1 = match s with parser
			| [< '(PClose,_); e1 = secure_expr >] -> e1
			| [< >] ->
				syntax_error (Expected [")"]) s (mk_null_expr (pos cond))
		in
		let e2 = (match s with parser
			| [< '(Kwd Else,_); e2 = secure_expr >] -> Some e2
			| [< >] ->
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
	| [< '(Kwd Return,p); s >] ->
		begin match s with parser
		| [< e = expr >] -> (EReturn (Some e),punion p (pos e))
		| [< >] ->
			if would_skip_display_position p true s then (EReturn (Some (mk_null_expr (punion_next p s))),p)
			else (EReturn None,p)
		end
	| [< '(Kwd Break,p) >] -> (EBreak,p)
	| [< '(Kwd Continue,p) >] -> (EContinue,p)
	| [< '(Kwd While,p1); '(POpen,_); cond = secure_expr; s >] ->
		let e = match s with parser
			| [< '(PClose,_); e = secure_expr >] -> e
			| [< >] ->
				syntax_error (Expected [")"]) s (mk_null_expr (pos cond))
		in
		(EWhile (cond,e,NormalWhile),punion p1 (pos e))
	| [< '(Kwd Do,p1); e = secure_expr; s >] ->
		begin match s with parser
			| [< '(Kwd While,_); '(POpen,_); cond = secure_expr; s >] ->
				let p2 = expect_unless_resume_p PClose s in
				(EWhile (cond,e,DoWhile),punion p1 p2)
			| [< >] ->
				syntax_error (Expected ["while"]) s e (* ignore do *)
		end
	| [< '(Kwd Switch,p1); e = secure_expr; s >] ->
		begin match s with parser
			| [< '(BrOpen,_); cases , def = parse_switch_cases e [] >] ->
				let p2 = match s with parser
					| [< '(BrClose,p2) >] -> p2
					| [< >] ->
						(* Ignore missing } if we are resuming and "guess" the last position. *)
						syntax_error (Expected ["}"]) s (pos (next_token s))
				in
				(ESwitch (e,cases,def),punion p1 p2)
			| [< >] ->
				syntax_error (Expected ["{"]) s (ESwitch(e,[],None),punion p1 (pos e))
		end
	| [< '(Kwd Try,p1); e = secure_expr; cl,p2 = parse_catches e [] (pos e) >] -> (ETry (e,cl),punion p1 p2)
	| [< '(IntInterval i,p1); e2 = expr >] -> make_binop OpInterval (EConst (Int (i, None)),p1) e2
	| [< '(Kwd Untyped,p1); e = secure_expr >] -> (EUntyped e,punion p1 (pos e))
	| [< '(Dollar v,p); s >] -> expr_next (EConst (Ident ("$"^v)),p) s
	| [< '(Kwd Inline,p); e = secure_expr >] -> make_meta Meta.Inline [] e p

and expr_next e1 s =
	try
		expr_next' e1 s
	with Stream.Error msg when !in_display ->
		handle_stream_error msg s;
		e1

and expr_next' e1 = parser
	| [< '(BrOpen,p1) when is_dollar_ident e1; eparam = expr; '(BrClose,p2); s >] ->
		(match fst e1 with
		| EConst(Ident n) -> expr_next (EMeta((Meta.from_string n,[],snd e1),eparam), punion p1 p2) s
		| _ -> die "" __LOC__)
	| [< '(Dot,p); e = parse_field e1 p >] -> e
	| [< '(QuestionDot,p); e = parse_field e1 p >] -> e
	| [< '(POpen,p1); e = parse_call_params (fun el p2 -> (ECall(e1,el)),punion (pos e1) p2) p1; s >] -> expr_next e s
	| [< '(BkOpen,p1); e2 = secure_expr; s >] ->
		let p2 = expect_unless_resume_p BkClose s in
		let e2 = check_signature_mark e2 p1 p2 in
		expr_next (EArray (e1,e2), punion (pos e1) p2) s
	| [< '(Arrow,pa); s >] ->
		let er = expr s in
		arrow_function (snd e1) [arrow_first_param e1 s] er s
	| [< '(Binop OpGt,p1); s >] ->
		(match s with parser
		| [< '(Binop OpGt,p2) when p1.pmax = p2.pmin; s >] ->
			(match s with parser
			| [< '(Binop OpGt,p3) when p2.pmax = p3.pmin >] ->
				(match s with parser
				| [< '(Binop OpAssign,p4) when p3.pmax = p4.pmin; e2 = expr >] -> make_binop (OpAssignOp OpUShr) e1 e2
				| [< e2 = secure_expr >] -> make_binop OpUShr e1 e2)
			| [< '(Binop OpAssign,p3) when p2.pmax = p3.pmin; e2 = expr >] -> make_binop (OpAssignOp OpShr) e1 e2
			| [< e2 = secure_expr >] -> make_binop OpShr e1 e2)
		| [< '(Binop OpAssign,p2) when p1.pmax = p2.pmin; s >] ->
			make_binop OpGte e1 (secure_expr s)
		| [< e2 = secure_expr >] ->
			make_binop OpGt e1 e2)
	| [< '(Binop op,_); e2 = secure_expr >] -> make_binop op e1 e2
	| [< '(Spread,_); e2 = secure_expr >] -> make_binop OpInterval e1 e2
	| [< '(Unop op,p) when is_postfix e1 op; s >] ->
		expr_next (EUnop (op,Postfix,e1), punion (pos e1) p) s
	| [< '(Question,_); e2 = expr; s >] ->
		begin match s with parser
		| [< '(DblDot,_); e3 = expr >] -> (ETernary (e1,e2,e3),punion (pos e1) (pos e3))
		| [< >] -> syntax_error (Expected [":"]) s e2
		end
	| [< '(Kwd In,_); e2 = expr >] ->
		make_binop OpIn e1 e2
	| [< '(Const (Ident "is"),p_is); t = parse_complex_type; s >] ->
		let p1 = pos e1 in
		let p2 = pos t in
		let e_is = EIs (e1,t), (punion p1 p2) in
		expr_next e_is s
	| [< >] -> e1

and parse_field e1 p s =
	check_resume p (fun () -> (EDisplay (e1,DKDot),p)) (fun () ->
		begin match s with parser
		| [< '(Kwd Macro,p2) when p.pmax = p2.pmin; s >] -> expr_next (EField (e1,"macro") , punion (pos e1) p2) s
		| [< '(Kwd Extern,p2) when p.pmax = p2.pmin; s >] -> expr_next (EField (e1,"extern") , punion (pos e1) p2) s
		| [< '(Kwd Function,p2) when p.pmax = p2.pmin; s >] -> expr_next (EField (e1,"function") , punion (pos e1) p2) s
		| [< '(Kwd New,p2) when p.pmax = p2.pmin; s >] -> expr_next (EField (e1,"new") , punion (pos e1) p2) s
		| [< '(Kwd k,p2) when !parsing_macro_cond && p.pmax = p2.pmin; s >] -> expr_next (EField (e1,s_keyword k) , punion (pos e1) p2) s
		| [< '(Const (Ident f),p2) when p.pmax = p2.pmin; s >] -> expr_next (EField (e1,f) , punion (pos e1) p2) s
		| [< '(Dollar v,p2); s >] -> expr_next (EField (e1,"$"^v) , punion (pos e1) p2) s
		| [< >] ->
			(* turn an integer followed by a dot into a float *)
			match e1 with
			| (EConst (Int (v, None)),p2) when p2.pmax = p.pmin -> expr_next (EConst (Float (v ^ ".", None)),punion p p2) s
			| _ -> serror()
		end
	)

and parse_guard = parser
	| [< '(Kwd If,p1); '(POpen,_); e = expr; '(PClose,_); >] ->
		e

and expr_or_var = parser
	| [< '(Kwd Var,p1); np = dollar_ident; >] -> EVars [mk_evar np],punion p1 (snd np)
	| [< '(Kwd Final,p1); np = dollar_ident; >] -> EVars [mk_evar ~final:true np],punion p1 (snd np)
	| [< e = secure_expr >] -> e

and parse_switch_cases eswitch cases = parser
	| [< '(Kwd Default,p1); '(DblDot,pdot); s >] ->
		let b,p2 = (block_with_pos [] p1 s) in
		let b = match b with
			| [] -> None,pdot
			| _ -> let p = punion p1 p2 in Some ((EBlock b,p)),p
		in
		let l , def = parse_switch_cases eswitch cases s in
		(match def with None -> () | Some _ -> syntax_error Duplicate_default ~pos:(Some p1) s ());
		l , Some b
	| [< '(Kwd Case,p1); el = psep Comma expr_or_var; eg = popt parse_guard; s >] ->
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
	| [< >] ->
		List.rev cases , None

and parse_catch etry = parser
	| [< '(Kwd Catch,p); '(POpen,_); name, pn = dollar_ident; s >] ->
		match s with parser
		| [< t,pt = parse_type_hint; '(PClose,_); e = secure_expr >] -> ((name,pn),(Some (t,pt)),e,punion p (pos e)),(pos e)
		| [< '(PClose,_); e = secure_expr >] -> ((name,pn),None,e,punion p (pos e)),(pos e)
		| [< '(_,p) >] -> error Missing_type p

and parse_catches etry catches pmax = parser
	| [< (catch,pmax) = parse_catch etry; s >] -> parse_catches etry (catch :: catches) pmax s
	| [< >] -> List.rev catches,pmax

and parse_call_params f p1 s =
	if not !in_display_file then begin
		let el = psep Comma expr s in
		match s with parser
		| [< '(PClose,p2) >] -> f el p2
		| [< >] ->
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
			match s with parser
			| [< '(PClose,p2) >] ->
				let e = check_signature_mark e p1 p2 in
				f (List.rev (e :: acc)) p2
			| [< '(Comma,p2) >] ->
				let e = check_signature_mark e p1 p2 in
				parse_next_param (e :: acc) p2
			| [< >] ->
				let p2 = next_pos s in
				syntax_error (Expected [",";")"]) s ();
				let e = check_signature_mark e p1 p2 in
				f (List.rev (e :: acc)) p2
		in
		match s with parser
		| [< '(PClose,p2) >] -> f [] p2
		| [< >] -> parse_next_param [] p1
	end

(* Tries to parse a toplevel expression and defaults to a null expression when in display mode.
   This function always accepts in display mode and should only be used for expected expressions,
   not accepted ones! *)
and secure_expr = parser
	| [< e = expr >] -> e
	| [< s >] ->
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
	| EField(e1,name) -> (EField(validate_macro_cond s e1,name), snd e)
	| ECall ((EConst (Ident _),_) as i, args) -> (ECall (i,List.map (validate_macro_cond s) args),snd e)
	| _ -> syntax_error (Custom ("Invalid conditional expression")) ~pos:(Some (pos e)) s ((EConst (Ident "false"),(pos e)))

let parse_macro_ident t p s =
	if t = "display" then Hashtbl.replace special_identifier_files (Path.UniqueKey.create p.pfile) t;
	let e = (EConst (Ident t),p) in
	None, e

let rec parse_macro_cond s =
	parsing_macro_cond := true;
	try
		let cond = (match s with parser
			| [< '(Const (Ident t),p) >] ->
				parse_macro_ident t p s
			| [< '(Const (String(s,qs)),p) >] ->
				None, (EConst (String(s,qs)),p)
			| [< '(Const (Int (i, s)),p) >] ->
				None, (EConst (Int (i, s)),p)
			| [< '(Const (Float (f, s)),p) >] ->
				None, (EConst (Float (f, s)),p)
			| [< '(Kwd k,p) >] ->
				parse_macro_ident (s_keyword k) p s
			| [< '(Unop op,p); tk, e = parse_macro_cond >] ->
				tk, make_unop op e p
			| [< '(POpen,p1); (e,p) = expr; '(PClose,p2) >] ->
				None, (EParenthesis(validate_macro_cond s (e,p)),punion p1 p2)) in
		parsing_macro_cond := false;
		cond
	with e ->
		parsing_macro_cond := false;
		raise e
