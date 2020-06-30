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

open Ast
open Type
open Common
open Typecore
open Error
open Globals

(* INLINE CONSTRUCTORS *)

(*
	First pass:
	Finds all inline objects and variables that alias them.
	Inline objects reference instances of TNew TObjectDecl and TArrayDecl.
	When an inline object is assigned to a variable, this variable is considered an alias of it.
	If an aliasing variable is assigned more than once then inlining will be cancelled for the inline
	object the variable would have aliased.
	The algorithm supports unassigned variables to be used as aliases 'var a; a = new Inl();'. For this
	reason variable declarations without assignment are tracked as IVKUnassigned inline variables.
	When an unassigned inline variable is assigned an alias it's scope is limited to that in which the
	assignment happened, after which the variable is set as "closed" and any appearance will cancel inlining.
	Fields of inline objects behave in the same way as unassigned inline variables, allowing nested object
	inlining.

	Second pass:
	Replaces inline objects with their inlined constructor expressions.
	Replaces field access of aliasing variables with the respective field inline variable or inlined methods.
	Because some replacements turn a single expression into many, this pass will map texpr into texpr list,
	which is converted into TBlocks by the caller as needed.
*)

type inline_object_ctor = {
	ioc_class : Type.tclass;
	ioc_tparams : Type.tparams;
	ioc_field : Type.tclass_field;
	ioc_forced : bool; (* Cancelling a forced constructor should produce an error *)
}

and inline_object_kind =
	| IOKCtor of inline_object_ctor
	| IOKStructure
	| IOKArray of int

(*
	inline_object
	Represents an instance of TNew TObjectDecl or TArrayDecl that has potential to be inlined.
	Wether the inlining is cancelled or not is decided during the analysis phase.
*)
and inline_object = {
	io_kind : inline_object_kind;
	io_expr : texpr; (* This is the inlined constructor expression *)
	io_pos : pos; (* The original position of the constructor expression *)
	mutable io_has_untyped : bool; (* Wether inlining this object would bring untyped expressions into the parent expression *)
	mutable io_cancelled : bool; (* Wether this inline object has been cancelled *)
	mutable io_declared : bool; (* Wether the variable declarations for this inline object's fields have already been output. (Used in final_map) *)
	mutable io_aliases : inline_var list; (* List of variables that are aliasing/referencing this inline object *)
	mutable io_fields : (string,inline_var) PMap.t; (* The fields that this inline object supports, fields are inline variables which might alias other inline_objects *)
	mutable io_inline_methods : texpr list; (* List of inlined method calls. Populated during analysis and consumed by the final_map phase *)
	mutable io_dependent_vars : tvar list; (* List of variables that should be cancelled if this inline object is cancelled *)
}

and inline_var_kind =
	| IVKField of
		inline_object *
		string * (* The field name *)
		texpr option (* If this is Some _ then this field is a constant. (Used for Array .length property) *)
	| IVKLocal

and inline_var_state =
	| IVSUnassigned (* The variable isn't yet assigned *)
	| IVSAliasing of inline_object (* The variable is aliasing an inline object *)
	| IVSCancelled (* The variable was cancelled and should no longer be considered for aliasing inline objects *)

(*
	inline_var
	Represents a local variable that is tracked by the inline constructor analysis.
	It's main purpose is to track variables that are considered aliases of inline objects.
	It also tracks all unassigned variables in the program.
*)
and inline_var = {
	iv_var : tvar;
	mutable iv_state : inline_var_state;
	mutable iv_kind : inline_var_kind;
	mutable iv_closed : bool (* Inline variables are marked as closed when the scope they were first assigned on ends, any appearance of this variable after it has been closed causes cancellation *)
}

and inline_object_field =
	| IOFInlineMethod of inline_object * inline_var * tclass * Type.tparams * tclass_field * tfunc
	| IOFInlineVar of inline_var
	| IOFNone

let inline_constructors ctx original_e =
	let inline_objs = ref IntMap.empty in
	let vars = ref IntMap.empty in
	let scoped_ivs = ref [] in
	let get_io (ioid:int) : inline_object = IntMap.find ioid !inline_objs in
	let get_iv (vid:int) : inline_var = IntMap.find (abs vid) !vars in
	let rec cancel_io (io:inline_object) (p:pos) : unit =
		if not io.io_cancelled then begin
			io.io_cancelled <- true;
			List.iter (fun iv -> cancel_iv iv p) io.io_aliases;
			PMap.iter (fun _ iv -> cancel_iv iv p) io.io_fields;
			match io.io_kind with
			| IOKCtor(ioc) ->
				List.iter (fun v -> if v.v_id < 0 then cancel_v v p) io.io_dependent_vars;
				if ioc.ioc_forced then begin
					display_error ctx "Forced inline constructor could not be inlined" io.io_pos;
					display_error ctx (compl_msg "Cancellation happened here") p;
				end
			| _ -> ()
		end
	and cancel_iv (iv:inline_var) (p:pos) : unit =
		if (iv.iv_state <> IVSCancelled) then begin
			let old = iv.iv_state in
			iv.iv_state <- IVSCancelled;
			begin match old with
			| IVSAliasing(io) -> cancel_io io p
			| _ -> ()
			end;
			let remove = match iv.iv_kind with
				| IVKField(io,_,_) -> io.io_cancelled
				| IVKLocal -> true
			in
			if remove then begin
				let v = iv.iv_var in
				vars := IntMap.remove (abs v.v_id) !vars;
				v.v_id <- (abs v.v_id);
			end
		end
	and cancel_v (v:tvar) (p:pos) : unit =
		try let iv = get_iv v.v_id in
			cancel_iv iv p
		with Not_found -> ()
	in
	let set_iv_alias iv io =
		if iv.iv_state <> IVSUnassigned || io.io_cancelled then begin
			cancel_io io io.io_pos;
			cancel_iv iv io.io_pos
		end else begin
			iv.iv_state <- IVSAliasing io;
			io.io_aliases <- iv :: io.io_aliases;
		end
	in
	let add (v:tvar) (kind:inline_var_kind) : inline_var =
		let iv = {
			iv_var = v;
			iv_state = IVSUnassigned;
			iv_kind = kind;
			iv_closed = false
		} in
		v.v_id <- -v.v_id;
		vars := IntMap.add (abs v.v_id) iv !vars;
		iv
	in
	let get_io_field (io:inline_object) (s:string) : inline_var =
		PMap.find s io.io_fields
	in
	let alloc_io_field_full (io:inline_object) (fname:string) (constexpr_option:texpr option) (t:t) (p:pos) : inline_var =
		let v = alloc_var VInlined fname t p in
		let iv = add v (IVKField (io,fname,constexpr_option)) in
		io.io_fields <- PMap.add fname iv io.io_fields;
		iv
	in
	let alloc_const_io_field (io:inline_object) (fname:string) (constexpr:texpr) : inline_var =
		let iv = alloc_io_field_full io fname (Some constexpr) constexpr.etype constexpr.epos in
		iv.iv_state <- IVSCancelled;
		iv
	in
	let alloc_io_field (io:inline_object) (fname:string) (t:t) (p:pos) : inline_var = alloc_io_field_full io fname None t p in
	let int_field_name i =
		if i < 0 then "n" ^ (string_of_int (-i))
		else (string_of_int i)
	in
	let is_extern_ctor c cf = c.cl_extern || has_class_field_flag cf CfExtern in
	let make_expr_for_list (el:texpr list) (t:t) (p:pos): texpr = match el with
		| [] -> mk (TBlock[]) ctx.t.tvoid p
		| [e] -> e
		| _ -> mk (TBlock (el)) t p
	in
	let make_expr_for_rev_list (el:texpr list) (t:t) (p:pos) : texpr = make_expr_for_list (List.rev el) t p in
	let curr_io_id = ref 0 in

	(*
		check_for_ctors
		Returns true if there are any potential inline objects in the expression.
		It is used to save work before running mark_ctors and analyze_aliases.
	*)
	let rec check_for_ctors ?(force_inline=false) e =
		let is_ctor, is_meta_inline = match e.eexpr, force_inline with
			| TMeta((Meta.Inline,_,_),_), _ ->
				false, true
			| TObjectDecl _, _
			| TArrayDecl _, _
			| TNew _, true ->
				true, false
			| TNew({ cl_constructor = Some ({cf_kind = Method MethInline; cf_expr = Some ({eexpr = TFunction _})} as cf)} as c,_,_), _ ->
				Inline.needs_inline ctx c.cl_extern cf, false
			| _ -> false, false
		in
		is_ctor || Type.check_expr (check_for_ctors ~force_inline:is_meta_inline) e
	in
	(*
		mark_ctors
		Finds all instances of potential inline objects in an expression and wraps them with metadata @:inlineObject(id).
		The id is incremented each time and is used later in the final_map phase to identify the correct inline_object.
	*)
	let rec mark_ctors ?(force_inline=false) e : texpr =
		let is_meta_inline = match e.eexpr with (TMeta((Meta.Inline,_,_),e)) -> true | _ -> false in
		let e = Type.map_expr (mark_ctors ~force_inline:is_meta_inline) e in
		let mark() =
			incr curr_io_id;
			let id_expr = (EConst(Int (string_of_int !curr_io_id)), e.epos) in
			let meta = (Meta.InlineObject, [id_expr], e.epos) in
			mk (TMeta(meta, e)) e.etype e.epos
		in
		match e.eexpr, force_inline with
			| TObjectDecl _, _
			| TArrayDecl _, _
			| TNew _, true ->
				mark()
			| TNew({ cl_constructor = Some ({cf_kind = Method MethInline; cf_expr = Some ({eexpr = TFunction _})} as cf)} as c,_,_), _ ->
				if Inline.needs_inline ctx c.cl_extern cf then mark()
				else e
			| _ -> e
	in

	(*
		analyze_aliases is the main work-horse of the constructor inliner analysis.
		It runs recursively over all expressions, it must do so in code execution order.
		The expression being analyzed should have been processed with mark_ctors before hand.

		returns: Some(inline variable) if the expression being analyzed returns an inline variable. None otherwise.

		seen_ctors: used to avoid infinite constructor inlining loops.

		captured: Wether the caller is ready to accept an inline variable. If analysis results in an inline
		variable and this argument is false then the inline variable must be cancelled before returning.

		is_lvalue: Wether the expression being analyzed is on the left side of an assignment.

		e: The expression to analyze
	*)
	let rec analyze_aliases (seen_ctors:tclass_field list) (captured:bool) (is_lvalue:bool) (e:texpr) : inline_var option =
		let mk_io ?(has_untyped=false) (iok : inline_object_kind) (id:int) (expr:texpr) : inline_object =
			let io = {
				io_kind = iok;
				io_expr = expr;
				io_pos = expr.epos;
				io_cancelled = false;
				io_declared = false;
				io_fields = PMap.empty;
				io_aliases = [];
				io_has_untyped = has_untyped;
				io_inline_methods = [];
				io_dependent_vars = [];
			} in
			inline_objs := IntMap.add id io !inline_objs;
			io
		in
		let analyze_aliases_in_lvalue e = analyze_aliases seen_ctors captured true e in
		let analyze_aliases_in_ctor cf captured e = analyze_aliases (cf::seen_ctors) captured false e in
		let analyze_aliases captured e = analyze_aliases seen_ctors captured false e in
		let get_io_inline_method io fname =
			begin match io.io_kind with
			| IOKCtor(ctor) ->
				begin try
					let f = PMap.find fname ctor.ioc_class.cl_fields in
					begin match f.cf_params, f.cf_kind, f.cf_expr with
					| [], Method MethInline, Some({eexpr = TFunction tf}) ->
						if Inline.needs_inline ctx ctor.ioc_class.cl_extern f then
							Some (ctor.ioc_class, ctor.ioc_tparams, f, tf)
						else
							None
					| _ -> None
					end
				with Not_found -> None
				end
			| _ -> None
			end
		in
		let handle_field_case ?(captured=false) ?(is_lvalue=false) efield ethis fname validate_io : inline_object_field =
			begin match analyze_aliases true ethis with
			| Some({iv_state = IVSAliasing io} as iv) when validate_io io ->
				begin match get_io_inline_method io fname with
				| Some(c, tl, cf, tf)->
					let method_type = apply_params c.cl_params tl cf.cf_type in
					let field_is_function = match efield.etype with | TFun _  -> true	| _ -> false in
					if field_is_function && Type.does_unify method_type efield.etype then
						IOFInlineMethod(io,iv,c,tl,cf,tf)
					else begin
						cancel_iv iv efield.epos;
						IOFNone
					end
				| None ->
					begin try
						let fiv = get_io_field io fname in
						if not (type_iseq_strict fiv.iv_var.v_type efield.etype) then raise Not_found;
						let iv_is_const iv = match iv.iv_kind with IVKField(_,_,Some(_)) -> true | _ -> false in
						if is_lvalue && iv_is_const fiv then raise Not_found;
						if fiv.iv_closed then raise Not_found;
						if not captured || (not is_lvalue && fiv.iv_state == IVSUnassigned) then cancel_iv fiv efield.epos;
						IOFInlineVar(fiv)
					with Not_found ->
						cancel_iv iv efield.epos;
						IOFNone
					end
				end
			| Some(iv) ->
				cancel_iv iv efield.epos;
				IOFNone
			| _ ->
				IOFNone
			end
		in
		let handle_field_case_no_methods efield ethis fname validate_io = match handle_field_case ~captured:captured ~is_lvalue:is_lvalue efield ethis fname validate_io with
			| IOFInlineMethod(io,_,_,_,_,_) -> cancel_io io efield.epos; None
			| IOFInlineVar(iv) -> Some(iv)
			| IOFNone -> None
		in
		let handle_default_case e =
			let old = !scoped_ivs in
			scoped_ivs := [];
			let f e = ignore(analyze_aliases false e) in
			Type.iter f e;
			List.iter (fun iv -> iv.iv_closed <- true) !scoped_ivs;
			scoped_ivs := old;
			None
		in
		let analyze_call_args call_args =
			let rec loop (vs, es) el = match el with
				| e :: el ->
					begin match e.eexpr with
					| TConst _ -> loop (vs, e::es) el
					| _ ->
						let v = alloc_var VGenerated "arg" e.etype e.epos in
						let decle = mk (TVar(v, Some e)) ctx.t.tvoid e.epos in
						ignore(analyze_aliases true decle);
						let mde = (Meta.InlineConstructorArgument (v.v_id, 0)), [], e.epos in
						let e = mk (TMeta(mde, e)) e.etype e.epos in
						loop (v::vs, e::es) el
					end
				| [] -> vs, (List.rev es)
			in
			loop ([],[]) call_args
		in
		let handle_inline_object_case (io_id:int) (force_inline:bool) (e:texpr) =
			match e.eexpr, e.etype with
			| TNew({ cl_constructor = Some ({cf_expr = Some ({eexpr = TFunction tf})} as cf)} as c,tl,pl),_
				when captured && not (List.memq cf seen_ctors) ->
				begin
					let argvs, pl = analyze_call_args pl in
					let _, cname = c.cl_path in
					let v = alloc_var VGenerated ("inl"^cname) e.etype e.epos in
					match Inline.type_inline_ctor ctx c cf tf (mk (TLocal v) (TInst (c,tl)) e.epos) pl e.epos with
					| Some inlined_expr ->
						let inlined_expr = mark_ctors inlined_expr in
						let has_untyped = (Meta.has Meta.HasUntyped cf.cf_meta) in
						let forced = is_extern_ctor c cf || force_inline in
						let io = mk_io (IOKCtor{ioc_class=c; ioc_tparams=tl; ioc_field=cf; ioc_forced=forced}) io_id inlined_expr ~has_untyped:has_untyped in
						io.io_dependent_vars <- argvs;
						let rec loop (c:tclass) (tl:t list) =
							let apply = apply_params c.cl_params tl in
							List.iter (fun cf ->
								match cf.cf_kind,cf.cf_expr with
								| Var _, _ ->
									let fieldt = apply cf.cf_type in
									ignore(alloc_io_field io cf.cf_name fieldt v.v_pos);
								| _ -> ()
							) c.cl_ordered_fields;
							match c.cl_super with
							| Some (c,tl) -> loop c (List.map apply tl)
							| None -> ()
						in loop c tl;
						let iv = add v IVKLocal in
						set_iv_alias iv io;
						ignore(analyze_aliases_in_ctor cf true io.io_expr);
						Some iv
					| _ ->
						List.iter (fun v -> cancel_v v v.v_pos) argvs;
						if is_extern_ctor c cf then display_error ctx "Extern constructor could not be inlined" e.epos;
						None
				end
			| TNew({ cl_constructor = Some ({cf_kind = Method MethInline; cf_expr = Some _} as cf)} as c,_,pl),_ when is_extern_ctor c cf ->
				error "Extern constructor could not be inlined" e.epos;
			| TObjectDecl fl, _ when captured && fl <> [] && List.for_all (fun((s,_,_),_) -> Lexer.is_valid_identifier s) fl ->
				let v = alloc_var VGenerated "inlobj" e.etype e.epos in
				let ev = mk (TLocal v) v.v_type e.epos in
				let el = List.map (fun ((s,_,_),e) ->
					let ef = mk (TField(ev,FDynamic s)) e.etype e.epos in
					let e = mk (TBinop(OpAssign,ef,e)) e.etype e.epos in
					e
				) fl in
				let io_expr = make_expr_for_list el ctx.t.tvoid e.epos in
				let io = mk_io (IOKStructure) io_id io_expr in
				List.iter (fun ((s,_,_),e) -> ignore(alloc_io_field io s e.etype v.v_pos)) fl;
				let iv = add v IVKLocal in
				set_iv_alias iv io;
				List.iter (fun e -> ignore(analyze_aliases true e)) el;
				Some iv
			| TArrayDecl el, TInst(_, [elemtype]) when captured ->
				let len = List.length el in
				let v = alloc_var VGenerated "inlarr" e.etype e.epos in
				let ev = mk (TLocal v) v.v_type e.epos in
				let el = List.mapi (fun i e ->
					let ef = mk (TArray(ev,(mk (TConst(TInt (Int32.of_int i))) e.etype e.epos))) elemtype e.epos in
					mk (TBinop(OpAssign,ef,e)) elemtype e.epos
				) el in
				let io_expr = make_expr_for_list el ctx.t.tvoid e.epos in
				let io = mk_io (IOKArray(len)) io_id io_expr in
				ignore(alloc_const_io_field io "length" (mk (TConst(TInt (Int32.of_int len))) ctx.t.tint e.epos));
				for i = 0 to len-1 do ignore(alloc_io_field io (int_field_name i) elemtype v.v_pos) done;
				let iv = add v IVKLocal in
				set_iv_alias iv io;
				List.iter (fun e -> ignore(analyze_aliases true e)) el;
				Some iv
			| _ ->
				handle_default_case e
		in
		match e.eexpr with
		| TMeta((Meta.Inline,_,_),{eexpr = TMeta((Meta.InlineObject, [(EConst(Int (id_str)), _)], _), e)}) ->
			let io_id = int_of_string id_str in
			handle_inline_object_case io_id true e
		| TMeta((Meta.InlineObject, [(EConst(Int (id_str)), _)], _), e) ->
			let io_id = int_of_string id_str in
			handle_inline_object_case io_id false e
		| TVar(v,None) -> ignore(add v IVKLocal); None
		| TVar(v,Some rve) ->
			begin match analyze_aliases true rve with
			| Some({iv_state = IVSAliasing(io)}) ->
				let iv = add v IVKLocal in
				set_iv_alias iv io;
			| _ -> ()
			end;
			None
		| TBinop(OpAssign, lve, rve) ->
			begin match analyze_aliases_in_lvalue lve with
			| Some({iv_state = IVSUnassigned} as iv) ->
				begin match analyze_aliases true rve with
				| Some({iv_state = IVSAliasing(io)}) ->
					scoped_ivs := iv :: !scoped_ivs;
					set_iv_alias iv io
				| _ -> cancel_iv iv lve.epos
				end;
				Some iv
			| Some(iv) -> cancel_iv iv e.epos; ignore(analyze_aliases false rve); None
			| _ -> ignore(analyze_aliases false rve); None
			end
		| TField(ethis, fa) ->
			handle_field_case_no_methods e ethis (field_name fa) (fun _ -> true)
		| TArray(ethis,{eexpr = TConst (TInt i)}) ->
			let i = Int32.to_int i in
			let validate_io io = match io.io_kind with IOKArray(l) when i >= 0 && i < l -> true | _ -> false in
			handle_field_case_no_methods e ethis (int_field_name i) validate_io
		| TLocal(v) when v.v_id < 0 ->
			let iv = get_iv v.v_id in
			if iv.iv_closed || not captured then cancel_iv iv e.epos;
			Some iv
		| TBlock(el) ->
			let rec loop = function
				| [e] -> analyze_aliases captured e
				| e::el -> ignore(analyze_aliases true e); loop (el)
				| [] -> None
			in loop el
		| TMeta((Meta.InlineConstructorArgument (vid,_),_,_),_) ->
			(* The contents have already been analyzed, so we must skip the wrapped expression *)
			(try
				let iv = get_iv vid in
				if iv.iv_closed || not captured then cancel_iv iv e.epos;
				Some(get_iv vid)
			with Not_found -> None)
		| TParenthesis e | TMeta(_,e) | TCast(e,None) ->
			analyze_aliases captured e
		| TCall(({eexpr=TField(ethis,fa)} as efield),call_args) ->
			let fname = field_name fa in
			let fiv = handle_field_case efield ethis fname (fun _ -> true) in
			begin match fiv with
			| IOFInlineMethod(io,io_var,c,tl,cf,tf) ->
				let argvs, pl = analyze_call_args call_args in
				io.io_dependent_vars <- io.io_dependent_vars @ argvs;
				io.io_has_untyped <- io.io_has_untyped or (Meta.has Meta.HasUntyped cf.cf_meta);
				begin match Inline.type_inline ctx cf tf (mk (TLocal io_var.iv_var) (TInst (c,tl)) e.epos) pl e.etype None e.epos true with
				| Some e ->
					let e = mark_ctors e in
					io.io_inline_methods <- io.io_inline_methods @ [e];
					begin match analyze_aliases captured e with
						| Some(iv) ->
							io.io_dependent_vars <- iv.iv_var :: io.io_dependent_vars;
							Some(iv)
						| None -> None
					end
				| None ->
					cancel_io io e.epos;
					None
				end
			| IOFInlineVar(iv) ->
				cancel_iv iv e.epos;
				List.iter (fun ca -> ignore(analyze_aliases false ca)) call_args;
				None
			| IOFNone ->
				List.iter (fun ca -> ignore(analyze_aliases false ca)) call_args;
				None
			end
		| _ ->
			handle_default_case e
	in
	let rec get_iv_var_decls (iv:inline_var) : texpr list =
		match iv with
		| {iv_state = IVSAliasing io} -> get_io_var_decls io
		| {iv_kind = IVKField(_,_,Some _)} -> []
		| {iv_state = IVSCancelled} ->
			let v = iv.iv_var in
			[(mk (TVar(v,None)) ctx.t.tvoid v.v_pos)]
		| _ -> []
	and get_io_var_decls (io:inline_object) : texpr list =
		if io.io_declared then [] else begin
			io.io_declared <- true;
			PMap.foldi (fun _ iv acc -> acc@(get_iv_var_decls iv)) io.io_fields []
		end
	in
	let included_untyped = ref false in
	let rec final_map ?(unwrap_block = false) (e:texpr) : ((texpr list) * (inline_object option)) =
		let default_case e =
			let f e =
				let (el,_) = final_map e in
				make_expr_for_rev_list el e.etype e.epos
			in
			([Type.map_expr f e], None)
		in
		(*
			field_case handles the final map of TField expressions.
			The last bool in the returned tuple indicates that the field was handled as an inlined method.
		*)
		let field_case ethis fa efield : ((texpr list) * (inline_object option) * bool) =
			let (tel, thiso) = final_map ethis in
			begin match thiso with
			| Some io ->
				let fname = field_name fa in
				begin try match get_io_field io fname with
				| {iv_state = IVSAliasing io} ->
					tel, Some io, false
				| iv ->
					let newexpr = match iv.iv_kind with
						| IVKField(_,_,Some constexpr) -> {constexpr with epos = e.epos}
						| _ -> mk (TLocal iv.iv_var) efield.etype efield.epos
					in
					(newexpr::tel), None, false
				with Not_found ->
					(* Since the field is not an inline variable then it must be an inlined method call *)
					match io.io_inline_methods with
					| e::el ->
						(* method fields will appear in the same order as they did during analysis, so we consume the first and remove it from the list *)
						io.io_inline_methods <- el;
						let el, io = final_map e in
						el @ tel, io, true
					| _ -> die "" __LOC__
				end
			| None ->
				let te = make_expr_for_rev_list tel ethis.etype ethis.epos in
				[mk (TField(te, fa)) efield.etype efield.epos], None, false
			end
		in
		match e.eexpr with
		| TMeta((Meta.InlineObject, [(EConst(Int (id_str)), _)], _), e) ->
			let io_id = int_of_string id_str in
			begin try
				let io = get_io io_id in
				if io.io_cancelled then raise Not_found;
				if io.io_has_untyped then included_untyped := true;
				let el,_ = final_map ~unwrap_block:true io.io_expr in
				let el = el @ get_io_var_decls io in
				(el,Some io)
			with Not_found ->
				default_case e
			end
		| TVar(v, None) when v.v_id < 0 ->
			(get_iv_var_decls (get_iv v.v_id)), None
		| TVar(v,Some e) when v.v_id < 0 ->
			let el = (get_iv_var_decls (get_iv v.v_id)) in
			let e,_ = (final_map ~unwrap_block:true e) in (e@el, None)
		| TBinop(OpAssign, lve, rve) ->
			let (lvel, lvo) = final_map lve in
			let (rvel, rvo) = final_map ~unwrap_block:true rve in
			begin match lvo with
			| Some(io) ->
				(rvel@lvel), lvo
			| None ->
				let rve = make_expr_for_rev_list rvel rve.etype rve.epos in
				begin match lvel with
				| [] -> die "" __LOC__
				| e::el ->
					let e = mk (TBinop(OpAssign, e, rve)) e.etype e.epos in
					(e::el), None
				end
			end
		| TCall(({eexpr=TField(ethis,fa)} as efield),call_args) ->
			begin match field_case ethis fa efield with
			| el, io, true -> (* the field was an inlined method *)
				el, io
			| el, _, false -> (* the field was a normal field access *)
				(*
					This is equivalent to `default_case e`, but field_case already run final_map
					on the TField expression so we must reuse those results.
				*)
				let f e =
					let (el,_) = final_map e in
					make_expr_for_rev_list el e.etype e.epos
				in
				let e1 = make_expr_for_rev_list el efield.etype efield.epos in
				let e = {e with eexpr = TCall(e1, List.map f call_args)} in
				[e], None
			end
		| TField(ethis, fa) ->
			let el, io, is_method = field_case ethis fa e in
			assert(not is_method);
			el, io
		| TArray(ethis, ({eexpr = TConst (TInt i)} as indexexpr)) ->
			let (tel, thiso) = final_map ethis in
			begin match thiso with
			| Some io ->
				let i = Int32.to_int i in
				let fname = int_field_name i in
				begin match get_io_field io fname with
				| {iv_state = IVSAliasing io} ->
					tel, Some io
				| iv ->
					let local = (mk (TLocal iv.iv_var) e.etype e.epos) in
					(local::tel), None
				end
			| None ->
				let te = make_expr_for_rev_list tel ethis.etype ethis.epos in
				[mk (TArray(te, indexexpr)) e.etype e.epos], None
			end
		| TLocal v when v.v_id < 0 ->
			begin match get_iv v.v_id with
			| {iv_state = IVSAliasing io} ->
				[], (Some io)
			| iv ->
				([mk (TLocal iv.iv_var) e.etype e.epos], None)
			end
		| TBlock el ->
			let rec loop acc el = match el with
				| [] -> acc, None
				| [e] ->
					let el',io = final_map ~unwrap_block:unwrap_block e in
					(el'@acc), io
				| e::el ->
					let el',_ = final_map ~unwrap_block:unwrap_block e in
					loop (el'@acc) el
			in
			let el, io = loop [] el in
			let el = if unwrap_block || Option.is_some io then el else [mk (TBlock (List.rev el)) e.etype e.epos] in
			el, io
		| TMeta((Meta.InlineConstructorArgument (_,_),_,_),e) ->
			final_map e
		| TParenthesis e' | TCast(e',None) | TMeta(_,e') ->
			let el, io = final_map e' in
			begin match io with
			| Some io ->
				el, Some io
			| None ->
				let e' = make_expr_for_rev_list el e'.etype e'.epos in
				[Type.map_expr (fun _ -> e') e], None
			end
		| _ -> default_case e
	in
	if not (check_for_ctors original_e) then original_e else
	let e = mark_ctors original_e in
	ignore(analyze_aliases [] false false e);
	if IntMap.for_all (fun _ io -> io.io_cancelled) !inline_objs then begin
		IntMap.iter (fun _ iv -> let v = iv.iv_var in if v.v_id < 0 then v.v_id <- -v.v_id ) !vars;
		original_e
	end else begin
		let el,_ = final_map e in
		let cf = ctx.curfield in
		if !included_untyped && not (Meta.has Meta.HasUntyped cf.cf_meta) then cf.cf_meta <- (Meta.HasUntyped,[],e.epos) :: cf.cf_meta;
		let e = make_expr_for_rev_list el e.etype e.epos in
		let rec get_pretty_name iv = match iv.iv_kind with
			| IVKField(io,fname,None) ->
				begin try
					let is_user_variable iv = match iv.iv_var.v_kind with VUser _ | VInlined -> true | _ -> false in
					let iv = List.find is_user_variable io.io_aliases in
					(get_pretty_name iv) ^ "_" ^ fname;
				with Not_found ->
					(get_pretty_name (List.hd io.io_aliases)) ^ "_" ^ fname;
				end
			| _ -> iv.iv_var.v_name
		in
		IntMap.iter (fun _ iv ->
			let v = iv.iv_var in
			if v.v_id < 0 then begin
				v.v_id <- -v.v_id;
				v.v_name <- get_pretty_name iv
			end
		) !vars;
		e
	end