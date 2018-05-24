(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

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

let popt f = parser
	| [< v = f >] -> Some v
	| [< >] -> None

let rec plist f = parser
	| [< v = f; l = plist f >] -> v :: l
	| [< >] -> []

let rec psep sep f = parser
	| [< v = f; s >] ->
		let rec loop = parser
			| [< '(sep2,_) when sep2 = sep; v = f; l = loop >] -> v :: l
			| [< >] -> []
		in
		v :: loop s
	| [< >] -> []

let pignore f =
	try
		ignore(f())
	with Stream.Error _ | Stream.Failure ->
		()

let expect_unless_resume_p f = parser
	| [< p = f >] -> p
	| [< s >] -> if do_resume() then pos (next_token s) else serror()

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

let lower_ident_or_macro = parser
	| [< '(Const (Ident i),p) when is_lower_ident i >] -> i
	| [< '(Kwd Macro,_) >] -> "macro"
	| [< '(Kwd Extern,_) >] -> "extern"

let property_ident = parser
	| [< i,p = ident >] -> i,p
	| [< '(Kwd Dynamic,p) >] -> "dynamic",p
	| [< '(Kwd Default,p) >] -> "default",p
	| [< '(Kwd Null,p) >] -> "null",p

let bropen = parser
	| [< '(BrOpen,_) >] -> ()

let comma = parser
	| [< '(Comma,_) >] -> ()

let colon = parser
	| [< '(DblDot,p) >] -> p

let semicolon s =
	if fst (last_token s) = BrClose then
		match s with parser
		| [< '(Semicolon,p) >] -> p
		| [< >] -> snd (last_token s)
	else
		match s with parser
		| [< '(Semicolon,p) >] -> p
		| [< s >] ->
			let pos = snd (last_token s) in
			if do_resume() then pos else error Missing_semicolon pos

let parsing_macro_cond = ref false

let rec	parse_file s =
	last_doc := None;
	match s with parser
	| [< '(Kwd Package,_); pack = parse_package; s >] ->
		begin match s with parser
		| [< '(Const(Ident _),p) when pack = [] >] -> error (Custom "Package name must start with a lowercase character") p
		| [< _ = semicolon; l = parse_type_decls pack []; '(Eof,_) >] -> pack , l
		end
	| [< l = parse_type_decls [] []; '(Eof,_) >] -> [] , l

and parse_type_decls pack acc s =
	try
		match s with parser
		| [< v = parse_type_decl; l = parse_type_decls pack (v :: acc) >] -> l
		| [< >] -> List.rev acc
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
	| Stream.Error _ when do_resume() ->
		ignore(resume false false s);
		parse_type_decls pack acc s

and parse_abstract doc meta flags = parser
	| [< '(Kwd Abstract,p1); name = type_name; tl = parse_constraint_params; st = parse_abstract_subtype; sl = plist parse_abstract_relations; '(BrOpen,_); fl, p2 = parse_class_fields false p1 >] ->
		let flags = List.map decl_flag_to_abstract_flag flags in
		let flags = (match st with None -> flags | Some t -> AbOver t :: flags) in
		({
			d_name = name;
			d_doc = doc;
			d_meta = meta;
			d_params = tl;
			d_flags = flags @ sl;
			d_data = fl;
		},punion p1 p2)

and parse_type_decl s =
	match s with parser
	| [< '(Kwd Import,p1) >] -> parse_import s p1
	| [< '(Kwd Using,p1) >] -> parse_using s p1
	| [< doc = get_doc; meta = parse_meta; c = parse_common_flags; s >] ->
		match s with parser
		| [< '(Kwd Enum,p1) >] ->
			begin match s with parser
			| [< a,p = parse_abstract doc ((Meta.Enum,[],null_pos) :: meta) c >] ->
				(EAbstract a,p)
			| [< name = type_name; tl = parse_constraint_params; '(BrOpen,_); l = plist parse_enum; '(BrClose,p2) >] ->
				(EEnum {
					d_name = name;
					d_doc = doc;
					d_meta = meta;
					d_params = tl;
					d_flags = List.map decl_flag_to_enum_flag c;
					d_data = l
				}, punion p1 p2)
			end
		| [< n , p1 = parse_class_flags; name = type_name; tl = parse_constraint_params >] ->
			let rec loop had_display p0 acc =
				let check_display p0 p1 =
					if not had_display && encloses_resume p1 then syntax_completion (if List.mem HInterface n then SCInterfaceRelation else SCClassRelation) p0
				in
				match s with parser
				| [< '(Kwd Extends,p1); t,b = parse_type_path_or_resume p1 >] ->
					check_display p0 {p1 with pmin = p0.pmax; pmax = p1.pmin};
					loop (had_display || b) (pos t) ((HExtends t) :: acc)
				| [< '(Kwd Implements,p1); t,b = parse_type_path_or_resume p1 >] ->
					check_display p0 {p1 with pmin = p0.pmax; pmax = p1.pmin};
					loop (had_display || b) (pos t) ((HImplements t) :: acc)
				| [< '(BrOpen,p1) >] ->
					check_display p0 {p1 with pmin = p0.pmax; pmax = p1.pmin};
					List.rev acc
				| [< >] ->
					if not (do_resume()) then serror() else begin
						check_display p0 {p1 with pmin = p0.pmax; pmax = (next_pos s).pmax};
						List.rev acc
					end
			in
			let hl = loop false (last_pos s) [] in
			let fl, p2 = parse_class_fields false p1 s in
			(EClass {
				d_name = name;
				d_doc = doc;
				d_meta = meta;
				d_params = tl;
				d_flags = List.map decl_flag_to_class_flag c @ n @ hl;
				d_data = fl;
			}, punion p1 p2)
		| [< '(Kwd Typedef,p1); name = type_name; tl = parse_constraint_params; '(Binop OpAssign,p2); t = parse_complex_type; s >] ->
			(match s with parser
			| [< '(Semicolon,_) >] -> ()
			| [< >] -> ());
			(ETypedef {
				d_name = name;
				d_doc = doc;
				d_meta = meta;
				d_params = tl;
				d_flags = List.map decl_flag_to_enum_flag c;
				d_data = t;
			}, punion p1 (pos t))
		| [< a,p = parse_abstract doc meta c >] ->
			EAbstract a,p


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
			(match s with parser
			| [< '(Const (Ident k),p) >] ->
				loop pn ((k,p) :: acc)
			| [< '(Kwd Macro,p) >] ->
				loop pn (("macro",p) :: acc)
			| [< '(Kwd Extern,p) >] ->
				loop pn (("extern",p) :: acc)
			| [< '(Binop OpMult,_); '(Semicolon,p2) >] ->
				p2, List.rev acc, IAll
			| [< >] ->
				serror());
		| [< '(Semicolon,p2) >] ->
			p2, List.rev acc, INormal
		| [< '(Kwd In,_); '(Const (Ident name),pname); '(Semicolon,p2) >] ->
			p2, List.rev acc, IAsName(name,pname)
		| [< '(Const (Ident "as"),_); '(Const (Ident name),pname); '(Semicolon,p2) >] ->
			p2, List.rev acc, IAsName(name,pname)
		| [< >] ->
			serror()
	in
	let p2, path, mode = (match s with parser
		| [< '(Const (Ident name),p) >] -> loop p [name,p]
		| [< >] -> if would_skip_resume p1 s then p1, [], INormal else serror()
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
			end
		| [< '(Semicolon,p2) >] ->
			p2,List.rev acc
	in
	let p2, path = (match s with parser
		| [< '(Const (Ident name),p) >] -> loop p [name,p]
		| [< >] -> if would_skip_resume p1 s then p1, [] else serror()
	) in
	(EUsing path,punion p1 p2)

and parse_abstract_relations s =
	match s with parser
	| [< '(Const (Ident "to"),_); t = parse_complex_type >] -> AbTo t
	| [< '(Const (Ident "from"),_); t = parse_complex_type >] -> AbFrom t

and parse_abstract_subtype s =
	match s with parser
	| [< '(POpen, _); t = parse_complex_type; '(PClose,_) >] -> Some t
	| [< >] -> None

and parse_package s = psep Dot lower_ident_or_macro s

and parse_class_fields tdecl p1 s =
	let l = parse_class_field_resume tdecl s in
	let p2 = (match s with parser
		| [< '(BrClose,p2) >] -> p2
		| [< >] -> if do_resume() then pos (last_token s) else serror()
	) in
	l, p2

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
		| Kwd Macro :: _ | Kwd Public :: _ | Kwd Static :: _ | Kwd Var :: _ | Kwd Final :: _ | Kwd Override :: _ | Kwd Dynamic :: _ | Kwd Inline :: _ when fdecl ->
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

and parse_class_field_resume tdecl s =
	if not (do_resume()) then
		plist parse_class_field s
	else try
		let c = parse_class_field s in
		c :: parse_class_field_resume tdecl s
	with Stream.Error _ | Stream.Failure ->
		if resume tdecl true s then parse_class_field_resume tdecl s else []

and parse_common_flags = parser
	| [< '(Kwd Private,_); l = parse_common_flags >] -> DPrivate :: l
	| [< '(Kwd Extern,_); l = parse_common_flags >] -> DExtern :: l
	| [< >] -> []

and parse_meta_argument_expr s =
	try
		expr s
	with Display e -> match fst e with
		| EDisplay(e,_) ->
			begin try
				type_path (string_list_of_expr_path_raise e) false (pos e)
			with Exit ->
				e
			end
		| _ ->
			e

and parse_meta_params pname s = match s with parser
	| [< '(POpen,p) when p.pmin = pname.pmax; params = psep Comma parse_meta_argument_expr; '(PClose,_); >] -> params
	| [< >] -> []

and parse_meta_entry = parser
	[< '(At,p1); s >] ->
		let meta = check_resume p1 (fun () -> Some (Meta.Last,[],p1)) (fun () -> None) in
		match s with parser
		| [< name,p = parse_meta_name p1; params = parse_meta_params p; s >] -> (name,params,punion p1 p)
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
		let meta = check_resume p (fun () -> Some (Meta.Last,p)) (fun() -> None) in
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
	| [< s >] -> if would_skip_resume p s then CTPath { tpackage = []; tname = ""; tparams = []; tsub = None },p else serror()

and parse_type_hint = parser
	| [< '(DblDot,p1); s >] ->
		let f () = parse_complex_type_at p1 s in
		check_resume_range p1 s
			(fun p2 ->
				let ct = CTPath magic_type_path in
				pignore(f);
				ct,null_pos
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
				| [< >] -> if do_resume() then t else serror()
			end;
		| [< >] ->
			if would_skip_resume p1 s then begin
				begin match s with parser
					| [< '(Comma,_) >] -> ()
					| [< >] -> ()
				end;
				{ tpackage = []; tname = ""; tparams = []; tsub = None },null_pos
			end else raise Stream.Failure

and parse_complex_type_inner allow_named = parser
	| [< '(POpen,p1); t = parse_complex_type; '(PClose,p2) >] -> CTParent t,punion p1 p2
	| [< '(BrOpen,p1); s >] ->
		(match s with parser
		| [< l,p2 = parse_type_anonymous false >] -> CTAnonymous l,punion p1 p2
		| [< t = parse_structural_extension; s>] ->
			let tl = t :: plist parse_structural_extension s in
			(match s with parser
			| [< l,p2 = parse_type_anonymous false >] -> CTExtend (tl,l),punion p1 p2
			| [< l,p2 = parse_class_fields true p1 >] -> CTExtend (tl,l),punion p1 p2)
		| [< l,p2 = parse_class_fields true p1 >] -> CTAnonymous l,punion p1 p2
		| [< >] -> serror())
	| [< '(Question,p1); t,p2 = parse_complex_type_inner allow_named >] ->
		CTOptional (t,p2),punion p1 p2
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
	if is_lower_ident name then
		(match s with parser
		| [< '(Dot,p) >] ->
			check_resume p
				(fun () -> raise (TypePath (List.rev (name :: pack),None,false,punion (match p0 with None -> p1 | Some p0 -> p0) p)))
				(fun () -> parse_type_path1 (match p0 with None -> Some p1 | Some _ -> p0) (name :: pack) s)
		| [< '(Semicolon,_) >] ->
			error (Custom "Type name should start with an uppercase letter") p1
		| [< >] -> serror())
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
			| [< '(Binop OpLt,_); l = psep Comma parse_type_path_or_const >] ->
				begin match s with parser
				| [<'(Binop OpGt,p2) >] -> l,p2
				| [< >] ->
					if do_resume() then l,pos (last_token s)
					else serror()
				end
			| [< >] -> [],p2
		) in
		{
			tpackage = List.rev pack;
			tname = name;
			tparams = params;
			tsub = sub;
		},punion (match p0 with None -> p1 | Some p -> p) p2

and type_name = parser
	| [< '(Const (Ident name),p) >] ->
		if is_lower_ident name then
			error (Custom "Type name should start with an uppercase letter") p
		else
			name,p
	| [< '(Dollar name,p) >] -> "$" ^ name,p

and parse_type_path_or_const = parser
	(* we can't allow (expr) here *)
	| [< '(BkOpen,p1); e = parse_array_decl p1 >] -> TPExpr (e)
	| [< t = parse_complex_type >] -> TPType t
	| [< '(Const c,p) >] -> TPExpr (EConst c,p)
	| [< '(Kwd True,p) >] -> TPExpr (EConst (Ident "true"),p)
	| [< '(Kwd False,p) >] -> TPExpr (EConst (Ident "false"),p)
	| [< e = expr >] -> TPExpr e
	| [< >] -> serror()

and parse_complex_type_next (t : type_hint) s =
	let make_fun t2 p2 = match t2 with
		| CTFunction (args,r) ->
			CTFunction (t :: args,r),punion (pos t) p2
		| _ ->
			CTFunction ([t] , (t2,p2)),punion (pos t) p2
	in
	match s with parser
	| [< '(Arrow,pa); s >] ->
		begin match s with parser
		| [< t2,p2 = parse_complex_type >] -> make_fun t2 p2
		| [< >] ->
			if would_skip_resume pa s then begin
				let ct = CTPath magic_type_path in
				make_fun ct null_pos
			end else serror()
		end
	| [< >] -> t

and parse_function_type_next tl p1 = parser
	| [< '(Arrow,pa); s >] ->
		begin match s with parser
		| [< tret = parse_complex_type_inner false >] -> CTFunction (tl,tret), punion p1 (snd tret)
		| [< >] -> if would_skip_resume pa s then begin
				let ct = (CTPath magic_type_path),null_pos in
				CTFunction (tl,ct), punion p1 pa
			end else serror()
		end
	| [< >] -> serror ()

and parse_type_anonymous opt = parser
	| [< '(Question,_) when not opt; s >] -> parse_type_anonymous true s
	| [< name, p1 = ident; t = parse_type_hint; s >] ->
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
		match s with parser
		| [< '(BrClose,p2) >] -> next [],p2
		| [< '(Comma,p2) >] ->
			(match s with parser
			| [< '(BrClose,p2) >] -> next [],p2
			| [< l,p2 = parse_type_anonymous false >] -> next l,punion p1 p2
			| [< >] -> serror());
		| [< >] ->
			if do_resume() then next [],p2
			else serror()

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
			ec_doc = doc;
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
			| [< e = toplevel_expr; s >] ->
				(try ignore(semicolon s) with Error (Missing_semicolon,p) -> !display_error Missing_semicolon p);
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
		name, punion p1 p2, FFun f, al

and parse_var_field_assignment = parser
	| [< '(Binop OpAssign,_); e = toplevel_expr; p2 = semicolon >] -> Some e , p2
	| [< p2 = semicolon >] -> None , p2
	| [< >] -> serror()

and parse_class_field s =
	let doc = get_doc s in
	match s with parser
	| [< meta = parse_meta; al = plist parse_cf_rights; s >] ->
		let name, pos, k, al = (match s with parser
		| [< '(Kwd Var,p1); name = dollar_ident; s >] ->
			begin match s with parser
			| [< '(POpen,_); i1 = property_ident; '(Comma,_); i2 = property_ident; '(PClose,_) >] ->
				let t = popt parse_type_hint s in
				let e,p2 = parse_var_field_assignment s in
				name, punion p1 p2, FProp (i1,i2,t, e), al
			| [< t = popt parse_type_hint; s >] ->
				let e,p2 = parse_var_field_assignment s in
				name, punion p1 p2, FVar (t,e), al
			end
		| [< '(Kwd Final,p1) >] ->
			begin match s with parser
			| [< name = dollar_ident; t = popt parse_type_hint; e,p2 = parse_var_field_assignment >] ->
				name,punion p1 p2,FVar(t,e),(al @ [AFinal,p1])
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
				| (AOverride,po) :: _ when would_skip_resume po s ->
					let f = {
						f_params = [];
						f_args = [];
						f_type = None;
						f_expr = None
					} in
					let _,p2 = next_token s in
					(magic_display_field_name,p2),punion po p2,FFun f,al
				| _ -> serror()
			end
		) in
		let pos = match al with
			| [] -> pos
			| (_,p) :: _ -> punion p pos
		in
		{
			cff_name = name;
			cff_doc = doc;
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

and parse_fun_name = parser
	| [< name,p = dollar_ident >] -> name,p
	| [< '(Kwd New,p) >] -> "new",p

and parse_fun_param s =
	let meta = parse_meta s in
	match s with parser
	| [< '(Question,_); name, pn = dollar_ident; t = popt parse_type_hint; c = parse_fun_param_value >] -> ((name,pn),true,meta,t,c)
	| [< name, pn = dollar_ident; t = popt parse_type_hint; c = parse_fun_param_value >] -> ((name,pn),false,meta,t,c)

and parse_fun_param_value = parser
	| [< '(Binop OpAssign,_); e = toplevel_expr >] -> Some e
	| [< >] -> None

and parse_fun_param_type = parser
	| [< '(Question,_); name = ident; t = parse_type_hint >] -> (name,true,t)
	| [< name = ident; t = parse_type_hint >] -> (name,false,t)

and parse_constraint_params = parser
	| [< '(Binop OpLt,_); l = psep Comma parse_constraint_param; '(Binop OpGt,_) >] -> l
	| [< >] -> []

and parse_constraint_param = parser
	| [< meta = parse_meta; name = type_name; s >] ->
		let params = (match s with parser
			| [< >] -> []
		) in
		let ctl = (match s with parser
			| [< '(DblDot,_); s >] ->
				(match s with parser
				| [< '(POpen,_); l = psep Comma parse_complex_type; '(PClose,_) >] -> l
				| [< t = parse_complex_type >] -> [t]
				| [< >] -> serror())
			| [< >] -> []
		) in
		{
			tp_name = name;
			tp_params = params;
			tp_constraints = ctl;
			tp_meta = meta;
		}

and parse_type_path_or_resume p1 s =
	let pnext = next_pos s in
	let check_resume exc =
		if do_resume() && is_resuming_file p1.pfile && encloses_resume (punion p1 pnext) then
			(magic_type_path,punion_next p1 s),true
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
	| [< '(Const (String name),p); s >] -> block2 (name,p,DoubleQuotes) (String name) p s
	| [< b = block [] >] -> EBlock b

and block2 name ident p s =
	match s with parser
	| [< '(DblDot,_) >] ->
		let e = try
			secure_expr s
		with Display e ->
			let acc = [name,e] in
			let e = EObjectDecl acc,punion p (pos e) in
			display e
		in
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
		let e = try f s with Display e -> display (EBlock (List.rev (e :: acc)),snd e) in
		block_with_pos (e :: acc) (pos e) s
	with
		| Stream.Failure ->
			List.rev acc,p
		| Stream.Error _ ->
			let tk , pos = next_token s in
			(!display_error) (Unexpected tk) pos;
			block_with_pos acc pos s
		| Error (e,p) ->
			(!display_error) e p;
			block_with_pos acc p s

and block_with_pos acc p s =
	block_with_pos' acc parse_block_elt p s

and parse_block_elt = parser
	| [< '(Kwd Var,p1); vl = parse_var_decls p1; p2 = semicolon >] ->
		(EVars vl,punion p1 p2)
	| [< '(Kwd Inline,p1); '(Kwd Function,_); e = parse_function p1 true; _ = semicolon >] -> e
	| [< e = expr; _ = semicolon >] -> e

and parse_obj_decl name e p0 s =
	let has_resume = ref false in
	let make_obj_decl el p1 =
		EObjectDecl (List.rev el),punion p0 p1
	in
	let rec loop p_end acc = match s with parser
		| [< '(Comma,p1); s >] ->
			if is_resuming p1 then has_resume := true;
			let next key = match s with parser
				| [< '(DblDot,_) >] ->
					let e = try
						secure_expr s
					with Display e ->
						let acc = (key,e) :: acc in
						let e = make_obj_decl acc (pos e) in
						display e
					in
					loop (pos e) ((key,e) :: acc)
				| [< >] -> serror()
			in
			begin match s with parser
				| [< name,p = ident >] -> next (name,p,NoQuotes)
				| [< '(Const (String name),p) >] -> next (name,p,DoubleQuotes)
				| [< >] ->
					let p2 = pos (next_token s) in
					if encloses_resume (punion p1 p2) then begin
						let e = make_obj_decl acc p2 in
						let e = EDisplay(e,DKStructure),(pos e) in
						display e
					end else
						acc,p_end
			end
		| [< >] -> acc,p_end
	in
	let el,p_end = loop p0 [name,e] in
	let e = make_obj_decl el p_end in
	if !has_resume then begin
		let e = EDisplay(e,DKStructure),(pos e) in
		display e
	end else
		e

and parse_array_decl p1 s =
	let secure_expr acc s = try
		expr s
	with Display e ->
		let acc = e :: acc in
		let e = EArrayDecl (List.rev acc),punion p1 (pos e) in
		display e
	in
	let resume_or_fail p1 =
		if do_resume () then begin
			let p = punion_next p1 s in
			[mk_null_expr p],p
		end else serror()
	in
	let el,p2 = match s with parser
		| [< '(BkClose,p2) >] -> [],p2
		| [< e0 = secure_expr [] >] ->
			let rec loop acc = match s with parser
				| [< '(Comma,pk) >] ->
					begin match s with parser
						| [< '(BkClose,p2) >] -> acc,p2
						| [< e = secure_expr acc >] -> loop (e :: acc)
						| [< >] -> resume_or_fail pk
					end
				| [< '(BkClose,p2) >] -> acc,p2
			in
			loop [e0]
		| [< >] -> resume_or_fail p1
	in
	EArrayDecl (List.rev el),punion p1 p2

and parse_var_decl_head = parser
	| [< name, p = dollar_ident; t = popt parse_type_hint >] -> (name,t,p)

and parse_var_assignment = parser
	| [< '(Binop OpAssign,p1); s >] ->
		Some (expr_or_fail (fun () -> error (Custom "expression expected after =") p1) s)
	| [< >] -> None

and parse_var_assignment_resume vl name pn t s =
	try
		let eo = parse_var_assignment s in
		((name,pn),t,eo)
	with Display e ->
		let v = ((name,pn),t,Some e) in
		let e = (EVars(List.rev (v :: vl)),punion pn (pos e)) in
		display e

and parse_var_decls_next vl = parser
	| [< '(Comma,p1); name,t,pn = parse_var_decl_head; s >] ->
		let v_decl = parse_var_assignment_resume vl name pn t s in
		parse_var_decls_next (v_decl :: vl) s
	| [< >] ->
		vl

and parse_var_decls p1 = parser
	| [< name,t,pn = parse_var_decl_head; s >] ->
		let v_decl = parse_var_assignment_resume [] name pn t s in
		List.rev (parse_var_decls_next [v_decl] s)
	| [< s >] -> error (Custom "Missing variable identifier") p1

and parse_var_decl = parser
	| [< name,t,pn = parse_var_decl_head; v_decl = parse_var_assignment_resume [] name pn t >] -> v_decl

and inline_function = parser
	| [< '(Kwd Inline,_); '(Kwd Function,p1) >] -> true, p1
	| [< '(Kwd Function,p1) >] -> false, p1

and parse_macro_expr p = parser
	| [< '(DblDot,_); t = parse_complex_type >] ->
		let _, to_type, _  = reify !in_macro in
		let t = to_type t p in
		(ECheckType (t,(CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tsub = Some "ComplexType"; tparams = [] },null_pos)),p)
	| [< '(Kwd Var,p1); vl = psep Comma parse_var_decl >] ->
		reify_expr (EVars vl,p1) !in_macro
	| [< d = parse_class None [] [] false >] ->
		let _,_,to_type = reify !in_macro in
		(ECheckType (to_type d,(CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tsub = Some "TypeDefinition"; tparams = [] },null_pos)),p)
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
			EFunction ((match name with None -> None | Some (name,_) -> Some (if inl then "inline_" ^ name else name)),f), punion p1 (pos e)
		in
		(try
			expr_next (make (secure_expr s)) s
		with
			Display e -> display (make e))

and arrow_expr = parser
	| [< '(Arrow,_); s >] -> try let e = expr s in e,false with Display e -> e,true
	| _ -> serror()

and arrow_function p1 al er =
	let make e =
		EFunction(None, { f_params = []; f_type = None; f_args = al; f_expr = Some (EReturn(Some e), (snd e));  }), punion p1 (pos e)
	in
	let e,display_error = er in
	if display_error then display (make e) else make e

and arrow_ident_checktype e = (match e with
	| EConst(Ident n),p -> (n,p),None
	| ECheckType((EConst(Ident n),p),(t,pt)),_ -> (n,p),(Some (t,pt))
	| _ -> serror())

and arrow_first_param e =
	(match fst e with
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
		| Display e ->
			display (make_meta name params e p)
		| Stream.Failure | Stream.Error _ when Path.unique_full_path p.pfile = (!resume_display).pfile ->
			let e = EConst (Ident "null"),null_pos in
			display (make_meta name params e p)
		end
	| [< '(BrOpen,p1); s >] ->
		(match s with parser
		| [< b = block1; s >] ->
			let p2 = match s with parser
				| [< '(BrClose,p2) >] -> p2
				| [< >] ->
					(* Ignore missing } if we are resuming and "guess" the last position. *)
					if do_resume() then pos (next_token s) else serror()
			in
			let e = (b,punion p1 p2) in
			(match b with
			| EObjectDecl _ -> expr_next e s
			| _ -> e)
		| [< >] ->
			check_resume p1 (fun() -> display (EDisplay ((EObjectDecl [],p1),DKStructure),p1)) serror;
		)
	| [< '(Kwd k,p) when !parsing_macro_cond; s >] ->
		expr_next (EConst (Ident (s_keyword k)), p) s
	| [< '(Kwd Macro,p); s >] ->
		parse_macro_expr p s
	| [< '(Kwd Var,p1); v = parse_var_decl >] -> (EVars [v],p1)
	| [< '(Const c,p); s >] -> expr_next (EConst c,p) s
	| [< '(Kwd This,p); s >] -> expr_next (EConst (Ident "this"),p) s
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
			| [< '(Const (Ident "is"),p_is); t = parse_type_path; '(PClose,p2); >] ->
				let e_is = make_is e t (punion p1 p2) p_is in
				expr_next (ECast (e_is,None),punion p1 (pos e_is)) s
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
			if do_resume() then (ENew(t,[]),punion p1 (pos t))
			else serror()
		end
	| [< '(POpen,p1); s >] -> (match s with parser
		| [< '(PClose,p2); er = arrow_expr; >] ->
			arrow_function p1 [] er
		| [< '(Question,p2); al = psep Comma parse_fun_param; '(PClose,_); er = arrow_expr; >] ->
			let al = (match al with | (np,_,_,topt,e) :: al -> (np,true,[],topt,e) :: al | _ -> assert false ) in
			arrow_function p1 al er
		| [<  e = expr; s >] -> (match s with parser
			| [< '(PClose,p2); s >] -> expr_next (EParenthesis e, punion p1 p2) s
			| [< '(Comma,pc); al = psep Comma parse_fun_param; '(PClose,_); er = arrow_expr; >] ->
				arrow_function p1 ((arrow_first_param e) :: al) er
			| [< t,pt = parse_type_hint; s >] -> (match s with parser
				| [< '(PClose,p2); s >] -> expr_next (EParenthesis (ECheckType(e,(t,pt)),punion p1 p2), punion p1 p2) s
				| [< '(Comma,pc); al = psep Comma parse_fun_param; '(PClose,_); er = arrow_expr; >] ->
					let (np,_) = arrow_ident_checktype e in
					arrow_function p1 ((np,false,[],(Some(t,pt)),None) :: al) er
				| [< '((Binop OpAssign),p2); ea1 = expr; s >] ->
					let with_args al er = (match fst e with
						| EConst(Ident n) ->
							arrow_function p1 (((n,snd e),true,[],(Some(t,pt)),(Some ea1)) :: al) er
						| _ -> serror())
					in
					(match s with parser
					| [< '(PClose,p2); er = arrow_expr; >] ->
						with_args [] er
					| [< '(Comma,pc); al = psep Comma parse_fun_param; '(PClose,_); er = arrow_expr; >] ->
						with_args al er
					| [< >] -> serror())
				| [< >] -> serror())
			| [< '(Const (Ident "is"),p_is); t = parse_type_path; '(PClose,p2); >] -> expr_next (make_is e t (punion p1 p2) p_is) s
			| [< >] -> serror())
		)
	| [< '(BkOpen,p1); e = parse_array_decl p1; s >] -> expr_next e s
	| [< '(Kwd Function,p1); e = parse_function p1 false; >] -> e
	| [< '(Unop op,p1) when is_prefix op; e = expr >] -> make_unop op e p1
	| [< '(Binop OpSub,p1); e = expr >] ->
		make_unop Neg e p1
	(*/* removed unary + : this cause too much syntax errors go unnoticed, such as "a + + 1" (missing 'b')
						without adding anything to the language
	| [< '(Binop OpAdd,p1); s >] ->
		(match s with parser
		| [< '(Const (Int i),p); e = expr_next (EConst (Int i),p) >] -> e
		| [< '(Const (Float f),p); e = expr_next (EConst (Float f),p) >] -> e
		| [< >] -> serror()) */*)
	| [< '(Kwd For,p); '(POpen,_); it = secure_expr; '(PClose,_); e = secure_expr >] -> (EFor (it,e),punion p (pos e))
	| [< '(Kwd If,p); '(POpen,_); cond = secure_expr; '(PClose,_); e1 = secure_expr; s >] ->
		let e2 = (match s with parser
			| [< '(Kwd Else,_); e2 = expr; s >] -> Some e2
			| [< >] ->
				match Stream.npeek 2 s with
				| [(Semicolon,_); (Kwd Else,_)] ->
					Stream.junk s;
					Stream.junk s;
					Some (secure_expr s)
				| _ ->
					None
		) in
		(EIf (cond,e1,e2), punion p (match e2 with None -> pos e1 | Some e -> pos e))
	| [< '(Kwd Return,p); e = popt toplevel_expr >] -> (EReturn e, match e with None -> p | Some e -> punion p (pos e))
	| [< '(Kwd Break,p) >] -> (EBreak,p)
	| [< '(Kwd Continue,p) >] -> (EContinue,p)
	| [< '(Kwd While,p1); '(POpen,_); cond = secure_expr; '(PClose,_); e = secure_expr >] -> (EWhile (cond,e,NormalWhile),punion p1 (pos e))
	| [< '(Kwd Do,p1); e = secure_expr; '(Kwd While,_); '(POpen,_); cond = secure_expr; '(PClose,_); s >] -> (EWhile (cond,e,DoWhile),punion p1 (pos e))
	| [< '(Kwd Switch,p1); e = secure_expr; '(BrOpen,_); cases , def = parse_switch_cases e []; '(BrClose,p2); s >] -> (ESwitch (e,cases,def),punion p1 p2)
	| [< '(Kwd Try,p1); e = secure_expr; cl,p2 = parse_catches e [] (pos e) >] -> (ETry (e,cl),punion p1 p2)
	| [< '(IntInterval i,p1); e2 = expr >] -> make_binop OpInterval (EConst (Int i),p1) e2
	| [< '(Kwd Untyped,p1); e = secure_expr >] -> (EUntyped e,punion p1 (pos e))
	| [< '(Dollar v,p); s >] -> expr_next (EConst (Ident ("$"^v)),p) s

and expr_next e1 s =
	try
		expr_next' e1 s
	with Stream.Error _ when do_resume() ->
		e1

and expr_next' e1 = parser
	| [< '(BrOpen,p1) when is_dollar_ident e1; eparam = expr; '(BrClose,p2); s >] ->
		(match fst e1 with
		| EConst(Ident n) -> expr_next (EMeta((Meta.from_string n,[],snd e1),eparam), punion p1 p2) s
		| _ -> assert false)
	| [< '(Dot,p); s >] ->
		check_resume p (fun () -> display (EDisplay (e1,DKDot),p)) (fun () -> ());
		(match s with parser
		| [< '(Kwd Macro,p2) when p.pmax = p2.pmin; s >] -> expr_next (EField (e1,"macro") , punion (pos e1) p2) s
		| [< '(Kwd Extern,p2) when p.pmax = p2.pmin; s >] -> expr_next (EField (e1,"extern") , punion (pos e1) p2) s
		| [< '(Kwd New,p2) when p.pmax = p2.pmin; s >] -> expr_next (EField (e1,"new") , punion (pos e1) p2) s
		| [< '(Const (Ident f),p2) when p.pmax = p2.pmin; s >] -> expr_next (EField (e1,f) , punion (pos e1) p2) s
		| [< '(Dollar v,p2); s >] -> expr_next (EField (e1,"$"^v) , punion (pos e1) p2) s
		| [< >] ->
			(* turn an integer followed by a dot into a float *)
			match e1 with
			| (EConst (Int v),p2) when p2.pmax = p.pmin -> expr_next (EConst (Float (v ^ ".")),punion p p2) s
			| _ -> serror())
	| [< '(POpen,p1); e = parse_call_params (fun el p2 -> (ECall(e1,el)),punion (pos e1) p2) p1; s >] -> expr_next e s
	| [< '(BkOpen,_); e2 = expr; '(BkClose,p2); s >] ->
		expr_next (EArray (e1,e2), punion (pos e1) p2) s
	| [< '(Arrow,pa); s >] ->
		let er = try let e = expr s in e,false with Display e -> e,true
		in arrow_function (snd e1) [arrow_first_param e1] er
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
	| [< '(Unop op,p) when is_postfix e1 op; s >] ->
		expr_next (EUnop (op,Postfix,e1), punion (pos e1) p) s
	| [< '(Question,_); e2 = expr; '(DblDot,_); e3 = expr >] ->
		(ETernary (e1,e2,e3),punion (pos e1) (pos e3))
	| [< '(Kwd In,_); e2 = expr >] ->
		make_binop OpIn e1 e2
	| [< >] -> e1

and parse_guard = parser
	| [< '(Kwd If,p1); '(POpen,_); e = expr; '(PClose,_); >] ->
		e

and expr_or_var = parser
	| [< '(Kwd Var,p1); name,p2 = dollar_ident; >] -> EVars [(name,p2),None,None],punion p1 p2
	| [< e = secure_expr >] -> e

and parse_switch_cases eswitch cases = parser
	| [< '(Kwd Default,p1); '(DblDot,_); s >] ->
		let b,p2 = (try block_with_pos [] p1 s with Display e -> display (ESwitch (eswitch,cases,Some (Some e,punion p1 (pos e))),punion (pos eswitch) (pos e))) in
		let b = match b with
			| [] -> None,p1
			| _ -> let p = punion p1 p2 in Some ((EBlock b,p)),p
		in
		let l , def = parse_switch_cases eswitch cases s in
		(match def with None -> () | Some _ -> error Duplicate_default p1);
		l , Some b
	| [< '(Kwd Case,p1); el = psep Comma expr_or_var; eg = popt parse_guard; s >] ->
		let pdot = expect_unless_resume_p colon s in
		if !was_auto_triggered then check_resume pdot (fun () -> ()) (fun () -> ());
		(match el with
		| [] -> error (Custom "case without a pattern is not allowed") p1
		| _ ->
			let b,p2 = (try block_with_pos [] p1 s with Display e -> display (ESwitch (eswitch,List.rev ((el,eg,Some e,punion p1 (pos e)) :: cases),None),punion (pos eswitch) (pos e))) in
			let b,p = match b with
				| [] ->
					let p2 = match eg with Some e -> pos e | None -> match List.rev el with (_,p) :: _ -> p | [] -> p1 in
					None,punion p1 p2
				| _ -> let p = punion p1 p2 in Some ((EBlock b,p)),p
			in
			parse_switch_cases eswitch ((el,eg,b,p) :: cases) s
		)
	| [< >] ->
		List.rev cases , None

and parse_catch etry = parser
	| [< '(Kwd Catch,p); '(POpen,_); name, pn = dollar_ident; s >] ->
		match s with parser
		| [< t,pt = parse_type_hint; '(PClose,_); e = secure_expr >] -> ((name,pn),(t,pt),e,punion p (pos e)),(pos e)
		| [< '(_,p) >] -> error Missing_type p

and parse_catches etry catches pmax = parser
	| [< (catch,pmax) = parse_catch etry; s >] -> parse_catches etry (catch :: catches) pmax s
	| [< >] -> List.rev catches,pmax

and parse_call_params f p1 s =
	if not (do_resume()) || not (is_resuming_file p1.pfile) then begin
		let el = psep Comma expr s in
		match s with parser
		| [< '(PClose,p2) >] -> f el p2
		| [< >] -> serror()
	end else begin
		let rec parse_next_param acc p1 =
			let e = try
				expr s
			with
			| Stream.Error _ | Stream.Failure ->
				mk_null_expr (punion_next p1 s)
			| Display e ->
				display (f (List.rev (e :: acc)) (pos e))
			in
			let check_signature_mark e p2 =
				if not (is_signature_display()) then e
				else begin
					let p = punion p1 p2 in
					if encloses_resume p then (mk_display_expr e DKMarked)
					else e
				end
			in
			match s with parser
			| [< '(PClose,p2) >] ->
				let e = check_signature_mark e p2 in
				f (List.rev (e :: acc)) p2
			| [< '(Comma,p2) >] ->
				let e = check_signature_mark e p2 in
				parse_next_param (e :: acc) p2
			| [< >] ->
				let p2 = next_pos s in
				let e = check_signature_mark e p2 in
				f (List.rev (e :: acc)) p2
		in
		match s with parser
		| [< '(PClose,p2) >] -> f [] p2
		| [< >] -> parse_next_param [] p1
	end

(* Parses an expression and catches Display exceptions. *)
and toplevel_expr s =
	try
		expr s
	with
		Display e -> e

(* Tries to parse a toplevel expression and defaults to a null expression when in display mode.
   This function always accepts in display mode and should only be used for expected expressions,
   not accepted ones! *)
and secure_expr s =
	match s with parser
	| [< e = toplevel_expr >] -> e
	| [< >] -> if do_resume() then mk_null_expr (punion_next (pos (last_token s)) s) else serror()

(* Like secure_expr, but with a custom fail function *)
and expr_or_fail fail s =
	match s with parser
	| [< e = expr >] -> e
	| [< >] -> if do_resume() then mk_null_expr (punion_next (pos (last_token s)) s) else fail()

let rec validate_macro_cond e = match fst e with
	| EConst (Ident _)
	| EConst (String _)
	| EConst (Int _)
	| EConst (Float _)
		-> e
	| EUnop (op,p,e1) -> (EUnop (op, p, validate_macro_cond e1), snd e)
	| EBinop (op,e1,e2) -> (EBinop(op, (validate_macro_cond e1), (validate_macro_cond e2)), snd e)
	| EParenthesis (e1) -> (EParenthesis (validate_macro_cond e1), snd e)
	| _ -> serror()

let parse_macro_ident t p s =
	if t = "display" then Hashtbl.replace special_identifier_files (Path.unique_full_path p.pfile) t;
	let e = (EConst (Ident t),p) in
	None, e

let rec parse_macro_cond s =
	parsing_macro_cond := true;
	try
		let cond = (match s with parser
			| [< '(Const (Ident t),p) >] ->
				parse_macro_ident t p s
			| [< '(Const (String s),p) >] ->
				None, (EConst (String s),p)
			| [< '(Const (Int i),p) >] ->
				None, (EConst (Int i),p)
			| [< '(Const (Float f),p) >] ->
				None, (EConst (Float f),p)
			| [< '(Kwd k,p) >] ->
				parse_macro_ident (s_keyword k) p s
			| [< '(Unop op,p); tk, e = parse_macro_cond >] ->
				tk, make_unop op e p
			| [< '(POpen,p1); (e,p) = expr; '(PClose,_) >] ->
				None, (EParenthesis(validate_macro_cond (e,p)),p1)) in
		parsing_macro_cond := false;
		cond
	with e ->
		parsing_macro_cond := false;
		raise e