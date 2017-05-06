(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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
	Inline objects reference instances of TNew TObjectDecl and TArrayDecl, identified a number
	assigned by order of appearance in the expression.
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
	Replace variables that alias inline objects with their respective field inline variables.
	Identify inline objects by order of appearance and replace them with their inlined constructor expressions.
	Replace field access of aliasing variables with the respective field inline variable.
	Because some replacements turn a single expression into many, this pass will map texpr into texpr list,
	which is converted into TBlocks by the caller as needed.
*)

type inline_object_kind = 
	| IOKCtor of tclass_field * bool * tvar list
	| IOKStructure
	| IOKArray of int

and inline_object = {
	io_kind : inline_object_kind;
	io_expr : texpr;
	io_pos : pos;
	io_has_untyped : bool;
	mutable io_cancelled : bool;
	mutable io_declared : bool;
	mutable io_aliases : inline_var list;
	mutable io_fields : (string,inline_var) PMap.t;
	mutable io_id_start : int;
	mutable io_id_end : int;
}

and inline_var_kind =
	| IVKField of inline_object * string * texpr option
	| IVKLocal

and inline_var_state =
	| IVSUnassigned
	| IVSAliasing of inline_object
	| IVSCancelled

and inline_var = {
	iv_var : tvar;
	mutable iv_state : inline_var_state;
	mutable iv_kind : inline_var_kind;
	mutable iv_closed : bool
}

let inline_constructors ctx e =
	let is_valid_ident s =
		try
			if String.length s = 0 then raise Exit;
			begin match String.unsafe_get s 0 with
				| 'a'..'z' | 'A'..'Z' | '_' -> ()
				| _ -> raise Exit
			end;
			for i = 1 to String.length s - 1 do
				match String.unsafe_get s i with
				| 'a'..'z' | 'A'..'Z' | '_' -> ()
				| '0'..'9' when i > 0 -> ()
				| _ -> raise Exit
			done;
			true
		with Exit ->
			false
	in
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
			| IOKCtor(_,isextern,vars) ->
				List.iter (fun v -> if v.v_id < 0 then cancel_v v p) vars;
				if isextern then begin
					display_error ctx "Extern constructor could not be inlined" io.io_pos;
					display_error ctx "Cancellation happened here" p;
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
		let v = alloc_var fname t p in
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
	let is_extern_ctor c cf = c.cl_extern || Meta.has Meta.Extern cf.cf_meta in
	let make_expr_for_list (el:texpr list) (t:t) (p:pos): texpr = match el with
		| [] -> mk (TBlock[]) ctx.t.tvoid p
		| [e] -> e
		| _ -> mk (TBlock (el)) t p
	in
	let make_expr_for_rev_list (el:texpr list) (t:t) (p:pos) : texpr = make_expr_for_list (List.rev el) t p in
	let current_io_id = ref 0 in
	let increment_io_id e = match e.eexpr with
		| TObjectDecl _ | TArrayDecl _ | TNew _ -> incr current_io_id
		| _ -> ()
	in
	let rec analyze_aliases (seen_ctors:tclass_field list) (captured:bool) (is_lvalue:bool) (e:texpr) : inline_var option =
		increment_io_id e;
		let mk_io ?(has_untyped=false) (iok : inline_object_kind) (id:int) (expr:texpr) : inline_object =
			let io = {
				io_kind = iok;
				io_expr = expr;
				io_pos = e.epos;
				io_cancelled = false;
				io_declared = false;
				io_fields = PMap.empty;
				io_aliases = [];
				io_id_start = id;
				io_id_end = id;
				io_has_untyped = has_untyped;
			} in
			inline_objs := IntMap.add id io !inline_objs;
			io
		in
		let analyze_aliases_in_lvalue e = analyze_aliases seen_ctors captured true e in
		let analyze_aliases_in_ctor cf captured e = analyze_aliases (cf::seen_ctors) captured false e in
		let analyze_aliases captured e = analyze_aliases seen_ctors captured false e in
		let handle_field_case te fname validate_io =
			begin match analyze_aliases true te with
			| Some({iv_state = IVSAliasing io} as iv) when validate_io io ->
				begin try
					let fiv = get_io_field io fname in
					if not (type_iseq_strict fiv.iv_var.v_type e.etype) then raise Not_found;
					let iv_is_const iv = match iv.iv_kind with IVKField(_,_,Some(_)) -> true | _ -> false in
					if is_lvalue && iv_is_const fiv then raise Not_found;
					if fiv.iv_closed then raise Not_found;
					if not captured || (not is_lvalue && fiv.iv_state == IVSUnassigned) then cancel_iv fiv e.epos;
					Some(fiv)
				with Not_found ->
					cancel_iv iv e.epos;
					None
				end
			| Some(iv) ->
				cancel_iv iv e.epos;
				None
			| _ -> None
			end
		in
		match e.eexpr, e.etype with
		| TNew({ cl_constructor = Some ({cf_kind = Method MethInline; cf_expr = Some ({eexpr = TFunction tf})} as cf)} as c,tl,pl),_
			when captured && not (List.memq cf seen_ctors) ->
			begin
				let io_id = !current_io_id in
				let rec loop (vs, decls, es) el = match el with
					| e :: el ->
						begin match e.eexpr with
						| TConst _ -> loop (vs, decls, e::es) el
						| _ -> 
							let v = alloc_var "arg" e.etype e.epos in
							let decle = mk (TVar(v, Some e)) ctx.t.tvoid e.epos in
							let io_id_start = !current_io_id in
							ignore(analyze_aliases true decle);
							let mde = (Meta.InlineConstructorArgument (v.v_id, io_id_start)), [], e.epos in
							let e = mk (TMeta(mde, e)) e.etype e.epos in
							loop (v::vs, decle::decls, e::es) el
						end
					| [] -> vs, (List.rev decls), (List.rev es)
				in
				let argvs, argvdecls, pl = loop ([],[],[]) pl in
				let _, cname = c.cl_path in
				let v = alloc_var ("inl"^cname) e.etype e.epos in
				match Optimizer.type_inline_ctor ctx c cf tf (mk (TLocal v) (TInst (c,tl)) e.epos) pl e.epos with
				| Some inlined_expr ->
					let has_untyped = (Meta.has Meta.HasUntyped cf.cf_meta) in
					let io = mk_io (IOKCtor(cf,is_extern_ctor c cf,argvs)) io_id inlined_expr ~has_untyped:has_untyped in
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
					io.io_id_start <- !current_io_id;
					ignore(analyze_aliases_in_ctor cf true io.io_expr);
					io.io_id_end <- !current_io_id;
					Some iv
				| _ ->
					List.iter (fun v -> cancel_v v v.v_pos) argvs;
					if is_extern_ctor c cf then display_error ctx "Extern constructor could not be inlined" e.epos;
					None
			end
		| TNew({ cl_constructor = Some ({cf_kind = Method MethInline; cf_expr = Some _} as cf)} as c,_,pl),_ when is_extern_ctor c cf ->
			error "Extern constructor could not be inlined" e.epos;
		| TObjectDecl fl, _ when captured && fl <> [] && List.for_all (fun(s,_) -> is_valid_ident s) fl ->
			let v = alloc_var "inlobj" e.etype e.epos in
			let ev = mk (TLocal v) v.v_type e.epos in
			let el = List.map (fun (s,e) ->
				let ef = mk (TField(ev,FDynamic s)) e.etype e.epos in
				let e = mk (TBinop(OpAssign,ef,e)) e.etype e.epos in
				e
			) fl in
			let io_expr = make_expr_for_list el ctx.t.tvoid e.epos in
			let io = mk_io (IOKStructure) !current_io_id io_expr in
			List.iter (fun (s,e) -> ignore(alloc_io_field io s e.etype v.v_pos)) fl;
			let iv = add v IVKLocal in
			set_iv_alias iv io;
			List.iter (fun e -> ignore(analyze_aliases true e)) el;
			io.io_id_end <- !current_io_id;
			Some iv
		| TArrayDecl el, TInst(_, [elemtype]) when captured ->
			let len = List.length el in
			let v = alloc_var "inlarr" e.etype e.epos in
			let ev = mk (TLocal v) v.v_type e.epos in
			let el = List.mapi (fun i e ->
				let ef = mk (TArray(ev,(mk (TConst(TInt (Int32.of_int i))) e.etype e.epos))) elemtype e.epos in
				mk (TBinop(OpAssign,ef,e)) elemtype e.epos
			) el in
			let io_expr = make_expr_for_list el ctx.t.tvoid e.epos in
			let io = mk_io (IOKArray(len)) !current_io_id io_expr in
			ignore(alloc_const_io_field io "length" (mk (TConst(TInt (Int32.of_int len))) ctx.t.tint e.epos));
			for i = 0 to len-1 do ignore(alloc_io_field io (int_field_name i) elemtype v.v_pos) done;
			let iv = add v IVKLocal in
			set_iv_alias iv io;
			List.iter (fun e -> ignore(analyze_aliases true e)) el;
			io.io_id_end <- !current_io_id;
			Some iv
		| TVar(v,None),_ -> ignore(add v IVKLocal); None
		| TVar(v,Some rve),_ ->
			begin match analyze_aliases true rve with
			| Some({iv_state = IVSAliasing(io)}) ->
				let iv = add v IVKLocal in
				set_iv_alias iv io;
			| _ -> ()
			end;
			None
		| TBinop(OpAssign, lve, rve),_ ->
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
		| TField(te, fa),_ ->
			handle_field_case te (field_name fa) (fun _ -> true)
		| TArray(te,{eexpr = TConst (TInt i)}),_ ->
			let i = Int32.to_int i in
			let validate_io io = match io.io_kind with IOKArray(l) when i >= 0 && i < l -> true | _ -> false in
			handle_field_case te (int_field_name i) validate_io
		| TLocal(v),_ when v.v_id < 0 ->
			let iv = get_iv v.v_id in
			if iv.iv_closed || not captured then cancel_iv iv e.epos;
			Some iv
		| TBlock(el),_ ->
			let rec loop = function
				| [e] -> analyze_aliases captured e
				| e::el -> ignore(analyze_aliases true e); loop (el)
				| [] -> None
			in loop el
		| TMeta((Meta.InlineConstructorArgument (vid,_),_,_),_),_ ->
			(try 
				let iv = get_iv vid in
				if iv.iv_closed || not captured then cancel_iv iv e.epos;
				Some(get_iv vid)
			with Not_found -> None)
		| TParenthesis e,_ | TMeta(_,e),_ | TCast(e,None),_ ->
			analyze_aliases captured e
		| _,_ ->
			let old = !scoped_ivs in
			scoped_ivs := [];
			let f e = ignore(analyze_aliases false e) in
			Type.iter f e;
			List.iter (fun iv -> iv.iv_closed <- true) !scoped_ivs;
			scoped_ivs := old;
			None
	in
	ignore(analyze_aliases [] false false e);
	current_io_id := 0;
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
		increment_io_id e;
		let default_case e = 
			let f e =
				let (el,_) = final_map e in
				make_expr_for_rev_list el e.etype e.epos
			in
			([Type.map_expr f e], None)
		in
		match e.eexpr with
		| TObjectDecl _ | TArrayDecl _ | TNew _ ->
			begin try
				let io = get_io !current_io_id in
				if io.io_cancelled then begin
					let result = default_case e in
					current_io_id := io.io_id_end;
					result
				end else begin
					if io.io_has_untyped then included_untyped := true;
					current_io_id := io.io_id_start;
					let el,_ = final_map ~unwrap_block:true io.io_expr in
					let el = el @ get_io_var_decls io in
					assert (!current_io_id = io.io_id_end);
					(el,Some io)
				end
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
				| [] -> assert false
				| e::el -> 
					let e = mk (TBinop(OpAssign, e, rve)) e.etype e.epos in
					(e::el), None
				end
			end
		| TField(te, fa) ->
			let (tel, thiso) = final_map te in
			begin match thiso with
			| Some io ->
				let fname = field_name fa in
				begin match get_io_field io fname with
				| {iv_state = IVSAliasing io} ->
					tel, Some io
				| iv ->
					let newexpr = match iv.iv_kind with
						| IVKField(_,_,Some constexpr) -> {constexpr with epos = e.epos}
						| _ -> mk (TLocal iv.iv_var) e.etype e.epos
					in
					(newexpr::tel), None
				end
			| None ->
				let te = make_expr_for_rev_list tel te.etype te.epos in
				[mk (TField(te, fa)) e.etype e.epos], None
			end
		| TArray(te, ({eexpr = TConst (TInt i)} as indexexpr)) ->
			let (tel, thiso) = final_map te in
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
				let te = make_expr_for_rev_list tel te.etype te.epos in
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
			let el = if unwrap_block then el else [mk (TBlock (List.rev el)) e.etype e.epos] in
			el, io
		| TMeta((Meta.InlineConstructorArgument (_,io_id_start),_,_),e) ->
			let old_io_id = !current_io_id in
			current_io_id := io_id_start;
			let result = final_map e in
			current_io_id := old_io_id;
			result
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
	if IntMap.for_all (fun _ io -> io.io_cancelled) !inline_objs then begin
		IntMap.iter (fun _ iv -> let v = iv.iv_var in if v.v_id < 0 then v.v_id <- -v.v_id ) !vars;
		e
	end else begin
		let el,_ = final_map e in
		let cf = ctx.curfield in
		if !included_untyped && not (Meta.has Meta.HasUntyped cf.cf_meta) then cf.cf_meta <- (Meta.HasUntyped,[],e.epos) :: cf.cf_meta;
		let e = make_expr_for_rev_list el e.etype e.epos in
		let rec get_pretty_name iv = match iv.iv_kind with
			| IVKField(io,fname,None) ->
				begin try
					let is_user_variable iv = Meta.has Meta.UserVariable iv.iv_var.v_meta in
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