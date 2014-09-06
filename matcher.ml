(*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

open Ast
open Common
open Type
open Typecore

type pvar = tvar * pos

type con_def =
	| CEnum of tenum * tenum_field
	| CConst of tconstant
	| CAny
	| CType of module_type
	| CArray of int
	| CFields of int * (string * tclass_field) list
	| CExpr of texpr

and con = {
	c_def : con_def;
	c_type : t;
	c_pos : pos;
}

and st_def =
	| SVar of tvar
	| SField of st * tclass_field
	| SEnum of st * tenum_field * int
	| SArray of st * int
	| STuple of st * int * int

and st = {
	st_def : st_def;
	st_type : t;
	st_pos : pos;
}

and dt =
	| Switch of st * (con * dt) list
	| Bind of ((tvar * pos) * st) list * dt
	| Goto of int
	| Expr of int
	| Guard of int * dt * dt option

(* Pattern *)

type pat_def =
	| PAny
	| PVar of pvar
	| PCon of con * pat list
	| POr of pat * pat
	| PBind of pvar * pat
	| PTuple of pat array

and pat = {
	p_def : pat_def;
	p_type : t;
	p_pos : pos;
}

type out = {
	mutable o_pos : pos;
	o_id : int;
	o_catch_all : bool;
	mutable o_num_paths : int;
}

type pat_vec = pat array * out
type pat_matrix = pat_vec list

(* Context *)

type pattern_ctx = {
	mutable pc_locals : (string, pvar) PMap.t;
	mutable pc_sub_vars : (string, pvar) PMap.t option;
	mutable pc_reify : bool;
	mutable pc_is_complex : bool;
}

type matcher = {
	ctx : typer;
	need_val : bool;
	dt_lut : dt DynArray.t;
	dt_cache : (dt,int) Hashtbl.t;
	mutable dt_count : int;
	mutable outcomes : out list;
	mutable toplevel_or : bool;
	mutable has_extractor : bool;
	mutable expr_map : (int,texpr * texpr option) PMap.t;
	mutable is_exhaustive : bool;
}

exception Not_exhaustive of pat * st
exception Unrecognized_pattern of Ast.expr

let arity con = match con.c_def with
	| CEnum (_,{ef_type = TFun(args,_)}) -> List.length args
	| CEnum _ -> 0
	| CConst _ -> 0
	| CType mt -> 0
	| CArray i -> i
	| CFields (i,_) -> i
	| CExpr _ -> 0
	| CAny -> 0

let mk_st def t p = {
	st_def = def;
	st_type = t;
	st_pos = p;
}

let mk_out mctx id e eg is_catch_all p =
	let out = {
		o_pos = p;
		o_id = id;
		o_catch_all = is_catch_all;
		o_num_paths = 0;
	} in
	mctx.outcomes <- out :: mctx.outcomes;
	mctx.expr_map <- PMap.add id (e,eg) mctx.expr_map;
	out

let clone_out mctx out p =
	let out = {out with o_pos = p; } in
	mctx.outcomes <- out :: mctx.outcomes;
	out

let get_guard mctx id =
	snd (PMap.find id mctx.expr_map)

let get_expr mctx id =
	fst (PMap.find id mctx.expr_map)

let mk_pat pdef t p = {
	p_def = pdef;
	p_type = t;
	p_pos = p;
}

let mk_con cdef t p = {
	c_def = cdef;
	c_type = t;
	c_pos = p;
}

let mk_con_pat cdef pl t p = mk_pat (PCon(mk_con cdef t p,pl)) t p

let mk_any t p = mk_pat PAny t p

let any = mk_any t_dynamic Ast.null_pos

let fake_tuple_type = TInst(mk_class null_module ([],"-Tuple") null_pos, [])

let mk_type_pat ctx mt t p =
	let rec loop = function
		| TClassDecl _ -> "Class"
		| TEnumDecl _ -> "Enum"
		| TAbstractDecl a when Meta.has Meta.RuntimeValue a.a_meta -> "Class"
		| TTypeDecl t ->
			begin match follow (monomorphs t.t_params t.t_type) with
				| TInst(c,_) -> loop (TClassDecl c)
				| TEnum(en,_) -> loop (TEnumDecl en)
				| TAbstract(a,_) -> loop (TAbstractDecl a)
				| _ -> error "Cannot use this type as a value" p
			end
		| _ -> error "Cannot use this type as a value" p
	in
	let tcl = Typeload.load_instance ctx {tname=loop mt;tpackage=[];tsub=None;tparams=[]} p true in
	let t2 = match tcl with TAbstract(a,_) -> TAbstract(a,[mk_mono()]) | _ -> assert false in
	unify ctx t t2 p;
	mk_con_pat (CType mt) [] t2 p

let mk_subs st con =
	let map = match follow st.st_type with
		| TInst(c,pl) -> apply_params c.cl_params pl
		| TEnum(en,pl) -> apply_params en.e_params pl
		| TAbstract(a,pl) -> apply_params a.a_params pl
		| _ -> fun t -> t
	in
	match con.c_def with
	| CFields (_,fl) -> List.map (fun (s,cf) -> mk_st (SField(st,cf)) (map cf.cf_type) st.st_pos) fl
	| CEnum (en,({ef_type = TFun _} as ef)) ->
		let rec loop t = match follow t with
			| TEnum(_,pl) -> pl
			| TAbstract({a_path = [],"EnumValue"},[]) -> []
			| TAbstract(a,pl) -> loop (Abstract.get_underlying_type a pl)
			| _ -> []
		in
		let pl = loop con.c_type in
		begin match apply_params en.e_params pl (monomorphs ef.ef_params ef.ef_type) with
			| TFun(args,r) ->
				ExtList.List.mapi (fun i (_,_,t) ->
					mk_st (SEnum(st,ef,i)) t st.st_pos
				) args
			| _ ->
				assert false
		end
	| CArray 0 -> []
	| CArray i ->
		let t = match follow con.c_type with TInst({cl_path=[],"Array"},[t]) -> t | _ -> assert false in
		ExtList.List.init i (fun i -> mk_st (SArray(st,i)) t st.st_pos)
	| CEnum _ | CConst _ | CType _ | CExpr _ | CAny ->
		[]

let get_tuple_params t = match t with
	| TFun(tl,tr) when tr == fake_tuple_type -> Some tl
	| _ -> None

(* Printing *)

let s_type = s_type (print_context())

let rec s_con con = match con.c_def with
	| CEnum(_,ef) -> ef.ef_name
	| CAny -> "_"
	| CConst c -> s_const c
	| CType mt -> s_type_path (t_path mt)
	| CArray i -> "[" ^(string_of_int i) ^ "]"
	| CFields (_,fl) -> String.concat "," (List.map (fun (s,_) -> s) fl)
	| CExpr e -> s_expr s_type e

let rec s_pat pat = match pat.p_def with
	| PVar (v,_) -> v.v_name
	| PCon (c,[]) -> s_con c
	| PCon (c,pl) -> s_con c ^ "(" ^ (String.concat "," (List.map s_pat pl)) ^ ")"
	| POr (pat1,pat2) -> s_pat pat1 ^ " | " ^ s_pat pat2
	| PAny -> "_"
	| PBind((v,_),pat) -> v.v_name ^ "=" ^ s_pat pat
	| PTuple pl -> "(" ^ (String.concat " " (Array.to_list (Array.map s_pat pl))) ^ ")"

let rec s_pat_vec pl =
	String.concat " " (Array.to_list (Array.map s_pat pl))

let rec s_pat_matrix pmat =
	String.concat "\n" (List.map (fun (pl,out) -> (s_pat_vec pl) ^ "->" ^ "") pmat)

let st_args l r v =
	(if l > 0 then (String.concat "," (ExtList.List.make l "_")) ^ "," else "")
	^ v ^
	(if r > 0 then "," ^ (String.concat "," (ExtList.List.make r "_")) else "")

let rec s_st st =
	(match st.st_def with
	| SVar v -> v.v_name
	| SEnum (st,ef,i) -> s_st st ^ "." ^ ef.ef_name ^ "." ^ (string_of_int i)
	| SArray (st,i) -> s_st st ^ "[" ^ (string_of_int i) ^ "]"
	| STuple (st,i,a) -> "(" ^ (st_args i (a - i - 1) (s_st st)) ^ ")"
	| SField (st,cf) -> s_st st ^ "." ^ cf.cf_name)

(* Pattern parsing *)

let unify_enum_field en pl ef t =
	let t2 = match follow ef.ef_type with
		| TFun(_,r) -> r
		| t2 -> t2
	in
	let t2 = (apply_params en.e_params pl (monomorphs ef.ef_params t2)) in
	Type.unify t2 t

let unify ctx a b p =
	try unify_raise ctx a b p with Error (Unify l,p) -> error (error_msg (Unify l)) p

let rec is_value_type = function
	| TMono r ->
		(match !r with None -> false | Some t -> is_value_type t)
	| TType (t,tl) ->
		is_value_type (apply_params t.t_params tl t.t_type)
	| TInst({cl_path=[],"String"},[]) ->
		true
	| TAbstract _ ->
		true
	| _ ->
		false

(* 	Determines if a type allows null-matching. This is similar to is_nullable, but it infers Null<T> on monomorphs,
	and enums are not considered nullable *)
let rec matches_null ctx t = match t with
	| TMono r ->
		(match !r with None -> r := Some (ctx.t.tnull (mk_mono())); true | Some t -> matches_null ctx t)
	| TType ({ t_path = ([],"Null") },[_]) ->
		true
	| TLazy f ->
		matches_null ctx (!f())
	| TType (t,tl) ->
		matches_null ctx (apply_params t.t_params tl t.t_type)
	| TFun _ | TEnum _ ->
		false
	| TAbstract (a,_) -> not (Meta.has Meta.NotNull a.a_meta)
	| _ ->
		true

let to_pattern ctx e t =
	let perror p = error "Unrecognized pattern" p in
	let verror n p = error ("Variable " ^ n ^ " must appear exactly once in each sub-pattern") p in
	let mk_var tctx s t p =
		let v = match tctx.pc_sub_vars with
			| Some vmap -> fst (try PMap.find s vmap with Not_found -> verror s p)
			| None -> alloc_var s t
		in
		unify ctx t v.v_type p;
		if PMap.mem s tctx.pc_locals then verror s p;
		tctx.pc_locals <- PMap.add s (v,p) tctx.pc_locals;
		v
	in
	let rec loop pctx e t =
		let p = pos e in
		match fst e with
		| ECheckType(e, CTPath({tpackage=["haxe";"macro"]; tname="Expr"})) ->
			let old = pctx.pc_reify in
			pctx.pc_reify <- true;
			let e = loop pctx e t in
			pctx.pc_reify <- old;
			e
		| EParenthesis e ->
			loop pctx e t
		| ECast(e1,None) ->
			loop pctx e1 t
		| EConst(Ident "null") ->
			if not (matches_null ctx t) then error ("Null-patterns are only allowed on nullable types (found " ^ (s_type t) ^ ")") p;
			mk_con_pat (CConst TNull) [] t p
		| EConst((Ident ("false" | "true") | Int _ | String _ | Float _) as c) ->
			let e = Codegen.type_constant ctx.com c p in
			unify ctx e.etype t p;
			let c = match e.eexpr with TConst c -> c | _ -> assert false in
			mk_con_pat (CConst c) [] t p
		| EMeta((Meta.Macro,[],_),(ECall (e1,args),_)) ->
			let path, field, args = Codegen.get_macro_path ctx e1 args p in
			begin match ctx.g.do_macro ctx MExpr path field args p with
				| Some e ->	loop pctx e t
				| None -> error "Macro failure" p
			end
		| EField _ ->
			let e = type_expr ctx e (WithType t) in
			let e = match Optimizer.make_constant_expression ctx ~concat_strings:true e with Some e -> e | None -> e in
			(match e.eexpr with
			| TConst c | TCast({eexpr = TConst c},None) ->
				mk_con_pat (CConst c) [] t p
			| TTypeExpr mt ->
				mk_type_pat ctx mt t p
			| TField(_, FStatic(_,cf)) when is_value_type cf.cf_type ->
				mk_con_pat (CExpr e) [] cf.cf_type p
			| TField(_, FEnum(en,ef)) ->
				begin try
					unify_enum_field en (List.map (fun _ -> mk_mono()) en.e_params) ef t
				with Unify_error l ->
					error (error_msg (Unify l)) p
				end;
				mk_con_pat (CEnum(en,ef)) [] t p
			| _ -> error "Constant expression expected" p)
		| ECall(ec,el) ->
			let ec = type_expr ctx ec (WithType t) in
			(match follow ec.etype with
			| TEnum(en,pl)
			| TFun(_,TEnum(en,pl)) ->
				let ef = match ec.eexpr with
					| TField (_,FEnum (_,f)) -> f
					| _ -> error ("Expected constructor for enum " ^ (s_type_path en.e_path)) p
				in
				let monos = List.map (fun _ -> mk_mono()) ef.ef_params in
				let tl,r = match apply_params en.e_params pl (apply_params ef.ef_params monos ef.ef_type) with
					| TFun(args,r) ->
						unify ctx r t p;
						List.map (fun (n,_,t) -> t) args,r
					| _ -> error "No arguments expected" p
				in
				let rec loop2 i el tl = match el,tl with
					| (EConst(Ident "_"),pany) :: [], t :: tl ->
						let pat = mk_pat PAny t_dynamic pany in
						(ExtList.List.make ((List.length tl) + 1) pat)
					| e :: el, t :: tl ->
						let pat = loop pctx e t in
						pat :: loop2 (i + 1) el tl
					| e :: _, [] ->
						error "Too many arguments" (pos e);
					| [],_ :: _ ->
						error "Not enough arguments" p;
					| [],[] ->
						[]
				in
				let el = loop2 0 el tl in
				List.iter2 (fun m (_,t) -> match follow m with TMono _ -> Type.unify m t | _ -> ()) monos ef.ef_params;
				pctx.pc_is_complex <- true;
				mk_con_pat (CEnum(en,ef)) el r p
			| _ -> perror p)
		| EConst(Ident "_") ->
			begin match get_tuple_params t with
			| Some tl ->
				let pl = List.map (fun (_,_,t) -> mk_any t p) tl in
				mk_pat (PTuple (Array.of_list pl)) t_dynamic p
			| None ->
				mk_any t p
			end
		| EConst(Ident s) ->
			begin try
				let ec = match follow t with
					| TEnum(en,pl) ->
						let ef = try
							PMap.find s en.e_constrs
						with Not_found when not (is_lower_ident s) ->
							error (string_error s en.e_names ("Expected constructor for enum " ^ (s_type_path en.e_path))) p
						in
						(match ef.ef_type with
							| TFun (args,_) ->
								let msg = Printf.sprintf "Enum constructor %s.%s requires parameters %s"
									(s_type_path en.e_path)
									ef.ef_name
									(String.concat ", " (List.map (fun (n,_,t) -> n ^ ":" ^ (s_type t)) args))
								in
								error msg p
							| _ -> ());
						let et = mk (TTypeExpr (TEnumDecl en)) (TAnon { a_fields = PMap.empty; a_status = ref (EnumStatics en) }) p in
						mk (TField (et,FEnum (en,ef))) (apply_params en.e_params pl ef.ef_type) p
					| TAbstract({a_impl = Some c} as a,_) when Meta.has Meta.Enum a.a_meta ->
						let cf = PMap.find s c.cl_statics in
						Type.unify (follow cf.cf_type) t;
						let e = begin match cf.cf_expr with
						| Some ({eexpr = TConst c | TCast({eexpr = TConst c},None)} as e) -> e
						| _ -> raise Not_found
						end in
						e
					| _ ->
						let old = ctx.untyped in
						ctx.untyped <- true;
						let e = try type_expr ctx e (WithType t) with _ -> ctx.untyped <- old; raise Not_found in
						ctx.untyped <- old;
						e
				in
				let ec = match Optimizer.make_constant_expression ctx ~concat_strings:true ec with Some e -> e | None -> ec in
				(match ec.eexpr with
					| TField (_,FEnum (en,ef)) ->
						begin try unify_raise ctx ec.etype t ec.epos with Error (Unify _,_) -> raise Not_found end;
						begin try
							unify_enum_field en (List.map (fun _ -> mk_mono()) en.e_params) ef t;
						with Unify_error l ->
							error (error_msg (Unify l)) p
						end;
						mk_con_pat (CEnum(en,ef)) [] t p
					| TConst c | TCast({eexpr = TConst c},None) ->
						begin try unify_raise ctx ec.etype t ec.epos with Error (Unify _,_) -> raise Not_found end;
						unify ctx ec.etype t p;
						mk_con_pat (CConst c) [] t p
					| TTypeExpr mt ->
						mk_type_pat ctx mt t p
					| _ ->
						raise Not_found);
			with Not_found ->
				begin match get_tuple_params t with
					| Some tl ->
						let s = String.concat "," (List.map (fun (_,_,t) -> s_type t) tl) in
						error ("Pattern should be tuple [" ^ s ^ "]") p
					| None ->
						if not (is_lower_ident s) && s.[0] <> '`' then error "Capture variables must be lower-case" p;
						let v = mk_var pctx s t p in
						mk_pat (PVar (v,p)) v.v_type p
				end
			end
		| (EObjectDecl fl) ->
			let is_matchable cf = match cf.cf_kind with Method _ -> false | _ -> true in
			let is_valid_field_name fields co n p =
				try
					let cf = PMap.find n fields in
					begin match co with
					| Some c when not (Typer.can_access ctx c cf false) -> error ("Cannot match against private field " ^ n) p
					| _ -> ()
					end
				with Not_found ->
					error ((s_type t) ^ " has no field " ^ n ^ " that can be matched against") p;
			in
			pctx.pc_is_complex <- true;
			let loop_fields fields f =
				let sl,pl,i = PMap.foldi (fun n cf (sl,pl,i) ->
					if not (is_matchable cf) then
						sl,pl,i
					else
						let pat = try
							if pctx.pc_reify && cf.cf_name = "pos" then raise Not_found;
							loop pctx (List.assoc cf.cf_name fl) (f cf)
						with Not_found ->
							(mk_any cf.cf_type p)
						in
						(n,cf) :: sl,pat :: pl,i + 1
				) fields ([],[],0) in
				mk_con_pat (CFields(i,sl)) pl t p
			in
			let fields,map = match follow t with
				| TAnon {a_fields = fields} ->
					fields,(fun cf -> cf.cf_type)
				| TInst(c,tl) ->
					c.cl_fields,(fun cf -> apply_params c.cl_params tl (monomorphs cf.cf_params cf.cf_type))
				| TAbstract({a_impl = Some c} as a,tl) ->
					let fields = List.fold_left (fun acc cf ->
						if Meta.has Meta.Impl cf.cf_meta then
							PMap.add cf.cf_name cf acc
						else acc
					) PMap.empty c.cl_ordered_statics in
					fields,(fun cf -> apply_params a.a_params tl (monomorphs cf.cf_params cf.cf_type))
				| _ ->
					error ((s_type t) ^ " cannot be matched against a structure") p
			in
			List.iter (fun (n,(_,p)) -> is_valid_field_name fields None n p) fl;
			loop_fields fields map
		| EArrayDecl [] ->
			mk_con_pat (CArray 0) [] t p
		| EArrayDecl el ->
			pctx.pc_is_complex <- true;
			begin match follow t with
				| TInst({cl_path=[],"Array"},[t2]) ->
					let pl = ExtList.List.mapi (fun i e ->
						loop pctx e t2
					) el in
					mk_con_pat (CArray (List.length el)) pl t p
				| TFun(tl,tr) when tr == fake_tuple_type ->
					let pl = try
						List.map2 (fun e (_,_,t) -> loop pctx e t) el tl
					with Invalid_argument _ ->
						error ("Invalid number of arguments: expected " ^ (string_of_int (List.length tl)) ^ ", found " ^ (string_of_int (List.length el))) p
					in
					mk_pat (PTuple (Array.of_list pl)) t_dynamic p
				| _ ->
					error ((s_type t) ^ " should be Array") p
			end
		| EBinop(OpAssign,(EConst(Ident s),p2),e1) ->
			let v = mk_var pctx s t p in
			let pat1 = loop pctx e1 t in
			mk_pat (PBind((v,p),pat1)) t p2
		| EBinop(OpOr,(EBinop(OpOr,e1,e2),p2),e3) ->
			loop pctx (EBinop(OpOr,e1,(EBinop(OpOr,e2,e3),p2)),p) t
		| EBinop(OpOr,e1,e2) ->
			let old = pctx.pc_locals in
			let pat1 = loop pctx e1 t in
			begin match pat1.p_def with
				| PAny | PVar _ ->
					display_error ctx "This pattern is unused" (pos e2);
					pat1
				| _ ->
					let pctx2 = {
						pc_sub_vars = Some pctx.pc_locals;
						pc_locals = old;
						pc_reify = pctx.pc_reify;
						pc_is_complex = pctx.pc_is_complex;
					} in
					let pat2 = loop pctx2 e2 t in
					pctx.pc_is_complex <- pctx2.pc_is_complex;
					PMap.iter (fun s (_,p) -> if not (PMap.mem s pctx2.pc_locals) then verror s p) pctx.pc_locals;
					mk_pat (POr(pat1,pat2)) pat2.p_type (punion pat1.p_pos pat2.p_pos);
			end
		| _ ->
			raise (Unrecognized_pattern e)
	in
	let pctx = {
		pc_locals = PMap.empty;
		pc_sub_vars = None;
		pc_reify = false;
		pc_is_complex = false;
	} in
	let x = loop pctx e t in
	x, pctx.pc_locals, pctx.pc_is_complex

let get_pattern_locals ctx e t =
	try
		let _,locals,_ = to_pattern ctx e t in
		PMap.foldi (fun n v acc -> PMap.add n v acc) locals PMap.empty
	with Unrecognized_pattern _ ->
		PMap.empty

(* Match compilation *)

let expr_eq e1 e2 = e1 == e2 || match e1.eexpr,e2.eexpr with
	| TConst ct1,TConst ct2 ->
		ct1 = ct2
	| TField(_,FStatic(c1,cf1)),TField(_,FStatic(c2,cf2)) ->
		c1 == c2 && cf1.cf_name = cf2.cf_name
	| _ ->
		false

let unify_con con1 con2 = match con1.c_def,con2.c_def with
	| CExpr e1, CExpr e2 ->
		expr_eq e1 e2
	| CConst c1,CConst c2 ->
		c1 = c2
	| CEnum(e1,ef1),CEnum(e2,ef2) ->
		e1 == e2 && ef1.ef_name = ef2.ef_name
	| CFields (i1,fl1),CFields (i2,fl2) ->
		(try
			List.iter (fun (s,_) -> if not (List.mem_assoc s fl1) then raise Not_found) fl2;
			true
		with Not_found ->
			false)
	| CType mt1,CType mt2 ->
		t_path mt1 = t_path mt2
	| CArray a1, CArray a2 ->
		a1 == a2
	| CAny, CAny ->
		true
	| _ ->
		false

let array_tl arr = Array.sub arr 1 (Array.length arr - 1)

let spec mctx con pmat =
	let a = arity con in
	let r = DynArray.create () in
	let add pv out =
		DynArray.add r (pv,out)
	in
	let rec loop2 pv out = match pv.(0).p_def with
		| PCon(c2,pl) when unify_con c2 con ->
			add (Array.append (Array.of_list pl) (array_tl pv)) out
		| PCon(c2,pl) ->
			()
		| PAny | PVar _->
			add (Array.append (Array.make a (mk_any (pv.(0).p_type) (pv.(0).p_pos))) (array_tl pv)) out
		| PBind(_,pat) ->
			loop2 (Array.append [|pat|] (array_tl pv)) out
		| PTuple tl ->
			loop2 tl out
		| POr _ ->
			assert false
	in
	let rec loop pmat = match pmat with
		| (pv,out) :: pl ->
			loop2 pv out;
			loop pl
		| [] ->
			()
	in
	loop pmat;
	DynArray.to_list r

let default mctx pmat =
	let r = DynArray.create () in
	let add pv out =
		DynArray.add r (pv,out)
	in
	let rec loop2 pv out = match pv.(0).p_def with
		| PCon _ ->
			()
		| PAny | PVar _->
			add (array_tl pv) out
		| PBind(_,pat) ->
			loop2 (Array.append [|pat|] (array_tl pv)) out
		| PTuple tl ->
			loop2 tl out
		| POr _ ->
			assert false
	in
	let rec loop pmat = match pmat with
		| (pv,out) :: pl ->
			loop2 pv out;
			loop pl;
		| [] ->
			()
	in
	loop pmat;
	DynArray.to_list r

let pick_column pmat =
	let rec loop i pv = if Array.length pv = 0 then -1 else match pv.(0).p_def with
		| PVar _ | PAny ->
			loop (i + 1) (array_tl pv)
		| PTuple pl ->
			loop i pl
		| _ ->
			i
	in
	loop 0 (fst (List.hd pmat))

let swap_pmat_columns i pmat =
	List.map (fun (pv,out) ->
		let pv = match pv with [|{p_def = PTuple pt}|] -> pt | _ -> pv in
		let tmp = pv.(i) in
		Array.set pv i pv.(0);
		Array.set pv 0 tmp;
		pv,out
	) pmat

let swap_columns i (row : 'a list) : 'a list =
	match row with
	| rh :: rt ->
		let rec loop count acc col = match col with
			| [] -> acc
			| ch :: cl when i = count ->
				ch :: (List.rev acc) @ [rh] @ cl
			| ch :: cl ->
				loop (count + 1) (ch :: acc) cl
		in
		loop 1 [] rt
	| _ ->
		[]

let expand_or mctx (pmat : pat_matrix) =
	let rec loop pat = match pat.p_def with
		| POr(pat1,pat2) ->
			let pat1 = loop pat1 in
			let pat2 = loop pat2 in
			pat1 @ pat2
		| PBind(v,pat1) ->
			let pat1 = loop pat1 in
			List.map (fun pat1 ->
				{pat with p_def = PBind(v,pat1)}
			) pat1
		| PTuple(pl) ->
			let pat1 = loop pl.(0) in
			List.map (fun pat1 ->
				let a1 = Array.copy pl in
				a1.(0) <- pat1;
				{pat with p_def = PTuple a1}
			) pat1
		| _ ->
			[pat]
	in
	let rec loop2 pmat = match pmat with
		| (pv,out) :: pmat ->
			let pat = loop pv.(0) in
			let pat' = ExtList.List.mapi (fun i pat ->
				(* TODO: This should really be active, but currently causes problems with or-patterns in
				   tuples (issue #2610). We will disable this for the 3.1.0 release, which means issue
				   #2508 is open again. *)
				(* let out = if i = 0 then out else clone_out mctx out pat.p_pos in *)
				let a1 = Array.copy pv in
				a1.(0) <- pat;
				a1,out
			) pat in
			pat' @ (loop2 pmat)
		| [] ->
			[]
	in
	loop2 pmat

let column_sigma mctx st pmat =
	let acc = ref [] in
	let bindings = ref [] in
	let unguarded = Hashtbl.create 0 in
	let add c g =
		if not (List.exists (fun c2 -> unify_con c2 c) !acc) then acc := c :: !acc;
		if not g then Hashtbl.replace unguarded c.c_def true;
	in
	let bind_st out st v =
		if not (List.exists (fun ((v2,p),_) -> v2.v_id == (fst v).v_id) !bindings) then bindings := (v,st) :: !bindings
	in
	let rec loop pmat = match pmat with
		| (pv,out) :: pr ->
			let rec loop2 out = function
				| PCon (c,_) ->
					add c ((get_guard mctx out.o_id) <> None);
				| PVar v ->
					bind_st out st v;
				| PBind(v,pat) ->
					bind_st out st v;
					loop2 out pat.p_def
				| PAny ->
					()
				| PTuple tl ->
					loop2 out tl.(0).p_def
				| POr _ ->
					assert false
			in
			loop2 out pv.(0).p_def;
			loop pr
		| [] ->
			()
	in
	loop pmat;
	List.rev_map (fun con -> con,not (Hashtbl.mem unguarded con.c_def)) !acc,!bindings

(* Determines if we have a Null<T>. Unlike is_null, this returns true even if the wrapped type is nullable itself. *)
let rec is_explicit_null = function
	| TMono r ->
		(match !r with None -> false | Some t -> is_null t)
	| TType ({ t_path = ([],"Null") },[t]) ->
		true
	| TLazy f ->
		is_null (!f())
	| TType (t,tl) ->
		is_null (apply_params t.t_params tl t.t_type)
	| _ ->
		false

let rec all_ctors mctx t =
	let h = ref PMap.empty in
	if is_explicit_null t then h := PMap.add (CConst TNull) Ast.null_pos !h;
	match follow t with
	| TAbstract({a_path = [],"Bool"},_) ->
		h := PMap.add (CConst(TBool true)) Ast.null_pos !h;
		h := PMap.add (CConst(TBool false)) Ast.null_pos !h;
		h,false
	| TAbstract({a_impl = Some c} as a,pl) when Meta.has Meta.Enum a.a_meta ->
		List.iter (fun cf ->
			ignore(follow cf.cf_type);
			if Meta.has Meta.Impl cf.cf_meta then match cf.cf_expr with
				| Some {eexpr = TConst c | TCast ({eexpr = TConst c},None)} -> h := PMap.add (CConst c) cf.cf_pos !h
				| _ -> ()
		) c.cl_ordered_statics;
		h,false
	| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) -> all_ctors mctx (Abstract.get_underlying_type a pl)
	| TInst({cl_path=[],"String"},_)
	| TInst({cl_path=[],"Array"},_) ->
		h,true
	| TEnum(en,pl) ->
		PMap.iter (fun _ ef ->
			let tc = monomorphs mctx.ctx.type_params t in
			try unify_enum_field en pl ef tc;
				h := PMap.add (CEnum(en,ef)) ef.ef_pos !h
			with Unify_error _ ->
				()
		) en.e_constrs;
		h,false
	| TAnon a ->
		h,true
	| TInst(_,_) ->
		h,true
	| _ ->
		h,true

let rec collapse_pattern pl = match pl with
	| pat :: [] ->
		pat
	| pat :: pl ->
		let pat2 = collapse_pattern pl in
		mk_pat (POr(pat,pat2)) pat.p_type (punion pat.p_pos pat2.p_pos)
	| [] ->
		assert false

let bind_remaining out pv stl =
	let rec loop stl pv =
		if Array.length pv = 0 then
			[]
		else
			match stl,pv.(0).p_def with
			| st :: stl,PAny ->
				loop stl (array_tl pv)
			| st :: stl,PVar v ->
				(v,st) :: loop stl (array_tl pv)
			| stl,PTuple pl ->
				loop stl pl
			| _ :: _,_->
				loop stl (array_tl pv)
			| [],_ ->
				[]
	in
	loop stl pv

let get_cache mctx dt =
	match dt with Goto _ -> dt | _ ->
		try
			Goto (Hashtbl.find mctx.dt_cache dt)
		with Not_found ->
			Hashtbl.replace mctx.dt_cache dt mctx.dt_count;
			mctx.dt_count <- mctx.dt_count + 1;
			DynArray.add mctx.dt_lut dt;
			dt

let rec compile mctx stl pmat toplevel =
	let guard id dt1 dt2 = get_cache mctx (Guard(id,dt1,dt2)) in
	let expr id = get_cache mctx (Expr id) in
	let bind bl dt = get_cache mctx (Bind(bl,dt)) in
	let switch st cl = get_cache mctx (Switch(st,cl)) in
	get_cache mctx (match pmat with
	| [] ->
		(match stl with
		| st :: stl ->
			let all,inf = all_ctors mctx st.st_type in
			let pl = PMap.foldi (fun cd p acc -> (mk_con_pat cd [] t_dynamic p) :: acc) !all [] in
			begin match pl,inf with
				| _,true
				| [],_ ->
					raise (Not_exhaustive(any,st))
				| _ ->
					raise (Not_exhaustive(collapse_pattern pl,st))
			end
		| _ ->
			(* This can happen in cases a value is required and all default cases are guarded (issue #3150).
			   Not a particularly elegant solution, may want to revisit this later. *)
			raise Exit)
	| ([|{p_def = PTuple pt}|],out) :: pl ->
		compile mctx stl ((pt,out) :: pl) toplevel
	| (pv,out) :: pl ->
		let i = pick_column pmat in
		if i = -1 then begin
			out.o_num_paths <- out.o_num_paths + 1;
			let bl = bind_remaining out pv stl in
			let dt = match (get_guard mctx out.o_id) with
				| None ->
					expr out.o_id
				| Some _ ->
					let dt = match pl,mctx.need_val with
						| [],false ->
							None
						| _ ->
							Some (compile mctx stl pl false)
					in
					guard out.o_id (expr out.o_id) dt
			in
			(if bl = [] then dt else bind bl dt)
		end else if i > 0 then begin
			let pmat = swap_pmat_columns i pmat in
			let stls = swap_columns i stl in
			compile mctx stls pmat toplevel
		end else begin
			let st_head,st_tail = match stl with st :: stl -> st,stl | _ -> assert false in
			let pmat = expand_or mctx pmat in
			let sigma,bl = column_sigma mctx st_head pmat in
			let all,inf = all_ctors mctx st_head.st_type in
			let cases = List.map (fun (c,g) ->
				if not g then all := PMap.remove c.c_def !all;
				let spec = spec mctx c pmat in
				let hsubs = mk_subs st_head c in
				let subs = hsubs @ st_tail in
				let dt = compile mctx subs spec false in
				c,dt
			) sigma in
			let def = default mctx pmat in
			let dt = match def,cases with
			| _ when List.exists (fun (c,_) -> match c.c_def with CFields _ -> true | _ -> false) cases ->
				switch st_head cases
			| _ when not inf && PMap.is_empty !all ->
				switch st_head cases
			| [],_ when inf && not mctx.need_val && toplevel ->
				(* ignore exhaustiveness, but mark context so we do not generate @:exhaustive metadata *)
				mctx.is_exhaustive <- false;
				switch st_head cases
			| [],_ when inf ->
				raise (Not_exhaustive(any,st_head))
			| [],_ ->
				let pl = PMap.foldi (fun cd p acc -> (mk_con_pat cd [] t_dynamic p) :: acc) !all [] in
				(* toplevel null can be omitted because the French dig runtime errors (issue #3054) *)
				if toplevel && (match pl with
					| [{p_def = PCon ({c_def = (CConst TNull)},_)}] -> true
					| _ -> false) then
						switch st_head cases
				else
					raise (Not_exhaustive(collapse_pattern pl,st_head))
			| def,[] ->
				compile mctx st_tail def false
			| def,_ ->
				let cdef = mk_con CAny t_dynamic st_head.st_pos in
				let def = try
					compile mctx st_tail def false
				with Exit ->
					raise (Not_exhaustive(any,st_head))
				in
				let cases = cases @ [cdef,def] in
				switch st_head cases
			in
			if bl = [] then dt else bind bl dt
		end)

let rec collapse_case el = match el with
	| e :: [] ->
		e
	| e :: el ->
		let e2 = collapse_case el in
		EBinop(OpOr,e,e2),punion (pos e) (pos e2)
	| [] ->
		assert false

let mk_const ctx p = function
	| TString s -> mk (TConst (TString s)) ctx.com.basic.tstring p
	| TInt i -> mk (TConst (TInt i)) ctx.com.basic.tint p
	| TFloat f -> mk (TConst (TFloat f)) ctx.com.basic.tfloat p
	| TBool b -> mk (TConst (TBool b)) ctx.com.basic.tbool p
	| TNull -> mk (TConst TNull) (ctx.com.basic.tnull (mk_mono())) p
	| _ -> error "Unsupported constant" p

let rec convert_st ctx st = match st.st_def with
	| SVar v -> mk (TLocal v) v.v_type st.st_pos
	| SField (sts,cf) ->
		let e = convert_st ctx sts in
		Typer.acc_get ctx (Typer.type_field ctx e cf.cf_name st.st_pos Typer.MGet) st.st_pos
	| SArray (sts,i) -> mk (TArray(convert_st ctx sts,mk_const ctx st.st_pos (TInt (Int32.of_int i)))) st.st_type st.st_pos
	| STuple (st,_,_) -> convert_st ctx st
	| SEnum(sts,ef,i) -> mk (TEnumParameter(convert_st ctx sts, ef, i)) st.st_type st.st_pos

let convert_con ctx con = match con.c_def with
	| CConst c -> mk_const ctx con.c_pos c
	| CType mt -> mk (TTypeExpr mt) t_dynamic con.c_pos
	| CExpr e -> e
	| CEnum(e,ef) -> mk_const ctx con.c_pos (TInt (Int32.of_int ef.ef_index))
	| CArray i -> mk_const ctx con.c_pos (TInt (Int32.of_int i))
	| CAny | CFields _ -> assert false

let convert_switch mctx st cases loop =
	let ctx = mctx.ctx in
	let e_st = convert_st ctx st in
	let p = e_st.epos in
	let mk_index_call () =
		let ttype = match follow (Typeload.load_instance ctx { tpackage = ["std"]; tname="Type"; tparams=[]; tsub = None} p true) with TInst(c,_) -> c | t -> assert false in
		let cf = PMap.find "enumIndex" ttype.cl_statics in
		let ec = (!type_module_type_ref) ctx (TClassDecl ttype) None p in
		let ef = mk (TField(ec, FStatic(ttype,cf))) (tfun [e_st.etype] ctx.t.tint) p in
		let e = make_call ctx ef [e_st] ctx.t.tint p in
		e
	in
	let wrap_exhaustive e =
		if mctx.is_exhaustive then
			mk (TMeta((Meta.Exhaustive,[],e.epos),e)) e.etype e.epos
		else
			e
	in
	let e = match follow st.st_type with
	| TEnum(_) ->
		wrap_exhaustive (mk_index_call())
	| TAbstract(a,pl) when (match Abstract.get_underlying_type a pl with TEnum(_) -> true | _ -> false) ->
		wrap_exhaustive (mk_index_call())
	| TInst({cl_path = [],"Array"},_) as t ->
		mk (TField (e_st,quick_field t "length")) ctx.t.tint p
	| TAbstract(a,_) when Meta.has Meta.Enum a.a_meta ->
		wrap_exhaustive (e_st)
	| TAbstract({a_path = [],"Bool"},_) ->
		wrap_exhaustive (e_st)
	| _ ->
		if List.exists (fun (con,_) -> match con.c_def with CEnum _ -> true | _ -> false) cases then
			mk_index_call()
		else
			e_st
	in
	let null = ref None in
	let def = ref None in
	let cases = List.filter (fun (con,dt) ->
		match con.c_def with
		| CConst TNull ->
			null := Some (loop dt);
			false
		| CAny ->
			def := Some (loop dt);
			false
		| _ ->
			true
	) cases in
	let dt = match cases with
		| [{c_def = CFields _},dt] -> loop dt
		| _ -> DTSwitch(e, List.map (fun (c,dt) -> convert_con ctx c, loop dt) cases, !def)
	in
	match !null with
	| None when is_explicit_null st.st_type ->
		let econd = mk (TBinop(OpNotEq,e_st,mk (TConst TNull) (mk_mono()) p)) ctx.t.tbool p in
		DTGuard(econd,dt,!def)
	| None ->
		dt
	| Some dt_null ->
		let econd = mk (TBinop(OpEq,e_st,mk (TConst TNull) (mk_mono()) p)) ctx.t.tbool p in
		DTGuard(econd,dt_null,Some dt)

(* Decision tree compilation *)

let transform_extractors eval cases p =
	let efail = (EThrow(EConst(Ident "false"),p)),p in
	let cfail = [(EConst (Ident "_"),p)],None,Some efail in
	let has_extractor = ref false in
	let rec loop cases = match cases with
		| (epat,eg,e) :: cases ->
			let ex = ref [] in
			let exc = ref 0 in
			let rec find_ex in_or e = match fst e with
				| EBinop(OpArrow,_,_) when in_or ->
					error "Extractors in or patterns are not allowed" (pos e)
				| EBinop(OpArrow, e1, e2) ->
					let ec = EConst (Ident ("__ex" ^ string_of_int (!exc))),snd e in
					let rec map_left e = match fst e with
						| EConst(Ident "_") -> ec
						| _ -> Ast.map_expr map_left e
					in
					let ecall = map_left e1 in
					ex := (ecall,e2) :: !ex;
					incr exc;
					has_extractor := true;
					ec
				| EBinop(OpOr,e1,e2) ->
					let e1 = find_ex true e1 in
					let e2 = find_ex true e2 in
					(EBinop(OpOr,e1,e2)),(pos e)
				| _ ->
					Ast.map_expr (find_ex in_or) e
			in
			let p = match e with None -> p | Some e -> pos e in
			let epat = match epat with
				| [epat] -> [find_ex false epat]
				| _ -> List.map (find_ex true) epat
			in
			let cases = loop cases in
			if !exc = 0 then
				(epat,eg,e) :: cases
			else begin
				let esubjects = EArrayDecl (List.map fst !ex),p in
				let case1 = [EArrayDecl (List.map snd !ex),p],eg,e in
				let cases2 = match cases with
					| [] -> [case1]
					| [[EConst (Ident "_"),_],_,e] -> case1 :: [[(EConst (Ident "_"),p)],None,e]
					| _ ->
						case1 :: [[(EConst (Ident "_"),p)],None,Some (ESwitch(eval,cases,None),p)]
				in
				let eswitch = (ESwitch(esubjects,cases2,None)),p in
				let case = epat,None,Some eswitch in
				begin match epat with
					| [EConst(Ident _),_] ->
						[case;cfail]
					| _ ->
						case :: cases
				end
			end
		| [] ->
			[]
	in
	let cases = loop cases in
	cases,!has_extractor

let extractor_depth = ref 0

let match_expr ctx e cases def with_type p =
	let need_val,with_type,tmono = match with_type with
		| NoValue -> false,NoValue,None
		| WithType t | WithTypeResume t when (match follow t with TMono _ -> true | _ -> false) ->
			(* we don't want to unify with each case individually, but instead at the end after unify_min *)
			true,Value,Some with_type
		| t -> true,t,None
	in
	(* turn default into case _ *)
	let cases = match cases,def with
		| [],None -> []
		| cases,Some def ->
			let p = match def with
				| None -> p
				| Some (_,p) -> p
			in
			cases @ [[(EConst(Ident "_")),p],None,def]
		| _ -> cases
	in
	let cases,has_extractor = transform_extractors e cases p in
	(* type subject(s) *)
	let array_match = ref false in
	let evals = match fst e with
		| EArrayDecl el | EParenthesis(EArrayDecl el,_) when (match el with [(EFor _ | EWhile _),_] -> false | _ -> true) ->
			array_match := true;
			List.map (fun e -> type_expr ctx e Value) el
		| _ ->
			let e = type_expr ctx e Value in
			begin match follow e.etype with
			(* TODO: get rid of the XmlType check *)
			| TEnum(en,_) when (en.e_path = ([],"XmlType")) || Meta.has Meta.FakeEnum en.e_meta ->
				raise Exit
			| TAbstract({a_path=[],("Int" | "Float" | "Bool")},_) | TInst({cl_path = [],"String"},_) when (Common.defined ctx.com Common.Define.NoPatternMatching) ->
				raise Exit;
			| _ ->
				()
			end;
			[e]
	in
	let var_inits = ref [] in
	let save = save_locals ctx in
	let a = List.length evals in
	(* turn subjects to subterms and handle variable initialization where necessary *)
	let stl = ExtList.List.mapi (fun i e ->
		let rec loop e = match e.eexpr with
			| TParenthesis e | TMeta(_,e) ->
				loop e
			| TLocal v ->
				mk_st (SVar v) e.etype e.epos
			| _ ->
				let v = gen_local ctx e.etype in
				var_inits := (v, Some e) :: !var_inits;
				ctx.locals <- PMap.add v.v_name v ctx.locals;
				mk_st (SVar v) e.etype e.epos
		in
		let st = loop e in
		if a = 1 then st else mk_st (STuple(st,i,a)) st.st_type st.st_pos
	) evals in
	let tl = List.map (fun st -> st.st_type) stl in
	(* create matcher context *)
	let mctx = {
		ctx = ctx;
		need_val = need_val;
		outcomes = [];
		toplevel_or = false;
		dt_lut = DynArray.create ();
		dt_cache = Hashtbl.create 0;
		dt_count = 0;
		has_extractor = has_extractor;
		expr_map = PMap.empty;
		is_exhaustive = true;
	} in
	(* flatten cases *)
	let cases = List.map (fun (el,eg,e) ->
		List.iter (fun e -> match fst e with EBinop(OpOr,_,_) -> mctx.toplevel_or <- true; | _ -> ()) el;
		match el with
			| [] ->
				let p = match e with None -> p | Some e -> pos e in
				error "case without a pattern is not allowed" p
			| _ ->
				collapse_case el,eg,e
	) cases in
	let is_complex = ref false in
	if mctx.has_extractor then incr extractor_depth;
	let add_pattern_locals (pat,locals,complex) =
		PMap.iter (fun n (v,p) -> ctx.locals <- PMap.add n v ctx.locals) locals;
		if complex then is_complex := true;
		pat
	in
	(* evaluate patterns *)
	let pl = ExtList.List.mapi (fun i (ep,eg,e) ->
		let save = save_locals ctx in
		(* type case patterns *)
		let pl,restore,with_type =
			try
				(* context type parameters are turned into monomorphs until the pattern has been typed *)
				let monos = List.map (fun _ -> mk_mono()) ctx.type_params in
				let t = match tl with [t] when not !array_match -> t | tl -> tfun tl fake_tuple_type in
				let t = apply_params ctx.type_params monos t in
				let pl = [add_pattern_locals (to_pattern ctx ep t)] in
				let old_ret = ctx.ret in
				ctx.ret <- apply_params ctx.type_params monos ctx.ret;
				let restore = PMap.fold (fun v acc ->
					(* apply context monomorphs to locals and replace them back after typing the case body *)
					let t = v.v_type in
					v.v_type <- apply_params ctx.type_params monos v.v_type;
					(fun () -> v.v_type <- t) :: acc
				) ctx.locals [fun() -> ctx.ret <- old_ret] in
				(* turn any still unknown types back to type parameters *)
				List.iter2 (fun m (_,t) -> match follow m with TMono _ -> Type.unify m t | _ -> ()) monos ctx.type_params;
				pl,restore,(match with_type with
					| WithType t -> WithType (apply_params ctx.type_params monos t)
					| WithTypeResume t -> WithTypeResume (apply_params ctx.type_params monos t)
					| _ -> with_type);
			with Unrecognized_pattern (e,p) ->
				error "Case expression must be a constant value or a pattern, not an arbitrary expression" p
		in
		let is_catch_all = match pl with
			| [{p_def = PAny | PVar _}] -> true
			| _ -> false
		in
		(* type case body *)
		let e = match e with
			| None -> mk (TBlock []) ctx.com.basic.tvoid (pos ep)
			| Some e ->
				let e = type_expr ctx e with_type in
				match with_type with
				| WithType t ->
					unify ctx e.etype t e.epos;
					Codegen.AbstractCast.check_cast ctx t e e.epos;
				| WithTypeResume t ->
					(try unify_raise ctx e.etype t e.epos with Error (Unify l,p) -> raise (Typer.WithTypeError (l,p)));
					Codegen.AbstractCast.check_cast ctx t e e.epos
				| _ -> e
		in
		(* type case guard *)
		let eg = match eg with
			| None -> None
			| Some e ->
				let eg = type_expr ctx e (WithType ctx.com.basic.tbool) in
				unify ctx eg.etype ctx.com.basic.tbool eg.epos;
				Some eg
		in
		List.iter (fun f -> f()) restore;
		save();
		let out = mk_out mctx i e eg is_catch_all (pos ep) in
		Array.of_list pl,out
	) cases in
	let check_unused () =
		let unused p =
			display_error ctx "This pattern is unused" p;
			let old_error = ctx.on_error in
			ctx.on_error <- (fun ctx s p -> ctx.on_error <- old_error; raise Exit);
			let check_expr e p =
				try begin match fst e with
						| EConst(Ident ("null" | "true" | "false")) -> ()
						| EConst(Ident _) ->
							ignore (type_expr ctx e Value);
							display_error ctx "Case expression must be a constant value or a pattern, not an arbitrary expression" (pos e)
						| _ -> ()
				end with Exit -> ()
			in
			let rec loop prev cl = match cl with
				| (_,Some _,_) :: cl -> loop prev cl
				| ((e,p2),_,_) :: cl ->
					if p2.pmin >= p.pmin then check_expr prev p else loop (e,p2) cl
				| [] ->
					check_expr prev p
			in
			(match cases with (e,_,_) :: cl -> loop e cl | [] -> assert false);
			ctx.on_error <- old_error;
		in
		let had_catch_all = ref false in
		List.iter (fun out ->
			if out.o_catch_all && not !had_catch_all then
				had_catch_all := true
			else if out.o_num_paths = 0 then begin
				unused out.o_pos;
				if mctx.toplevel_or then begin match evals with
					| [{etype = t}] when (match follow t with TAbstract({a_path=[],"Int"},[]) -> true | _ -> false) ->
						display_error ctx "Note: Int | Int is an or-pattern now" p;
					| _ -> ()
				end;
			end
		) (List.rev mctx.outcomes);
	in
	let dt = try
		(* compile decision tree *)
		compile mctx stl pl true
	with Not_exhaustive(pat,st) ->
		let rec s_st_r top pre st v = match st.st_def with
			| SVar v1 ->
				if not pre then v else begin try
					let e = match List.assoc v1 !var_inits with Some e -> e | None -> assert false in
					(Type.s_expr_pretty "" (Type.s_type (print_context())) e) ^ v
				with Not_found ->
					v1.v_name ^ v
				end
			| STuple(st,i,a) ->
				let r = a - i - 1 in
				Printf.sprintf "[%s]" (st_args i r (s_st_r top false st v))
			| SArray(st,i) ->
				s_st_r false true st (Printf.sprintf "[%i]%s" i (if top then " = " ^ v else v))
			| SField({st_def = SVar v1},cf) when v1.v_name.[0] = '`' ->
				cf.cf_name ^ (if top then " = " ^ v else v)
			| SField(st,cf) ->
				s_st_r false true st (Printf.sprintf ".%s%s" cf.cf_name (if top then " = " ^ v else v))
			| SEnum(st,ef,i) ->
				let len = match follow ef.ef_type with TFun(args,_) -> List.length args | _ -> 0 in
				s_st_r false false st (Printf.sprintf "%s(%s)" ef.ef_name (st_args i (len - 1 - i) v))
		in
		let pat = match follow st.st_type with
			| TAbstract({a_impl = Some cl} as a,_) when Meta.has Meta.Enum a.a_meta ->
				let rec s_pat pat = match pat.p_def with
					| PCon ({c_def = CConst c},[]) when c <> TNull ->
						let cf = List.find (fun cf ->
							match cf.cf_expr with
							| Some ({eexpr = TConst c2 | TCast({eexpr = TConst c2},None)}) -> c = c2
							| _ -> false
						) cl.cl_ordered_statics in
						cf.cf_name
					| PVar (v,_) -> v.v_name
					| PCon (c,[]) -> s_con c
					| PCon (c,pl) -> s_con c ^ "(" ^ (String.concat "," (List.map s_pat pl)) ^ ")"
					| POr (pat1,pat2) -> s_pat pat1 ^ " | " ^ s_pat pat2
					| PAny -> "_"
					| PBind((v,_),pat) -> v.v_name ^ "=" ^ s_pat pat
					| PTuple pl -> "(" ^ (String.concat " " (Array.to_list (Array.map s_pat pl))) ^ ")"
				in
				s_pat pat
			| _ ->
				s_pat pat
		in
		error ("Unmatched patterns: " ^ (s_st_r true false st pat)) st.st_pos
	in
	save();
	(* check for unused patterns *)
	if !extractor_depth = 0 then check_unused();
	if mctx.has_extractor then decr extractor_depth;
	(* determine type of switch statement *)
	let t = if not need_val then
		mk_mono()
	else match with_type with
		| WithType t | WithTypeResume t -> t
		| _ -> try Typer.unify_min_raise ctx (List.rev_map (fun (_,out) -> get_expr mctx out.o_id) (List.rev pl)) with Error (Unify l,p) -> error (error_msg (Unify l)) p
	in
	(* unify with expected type if necessary *)
	begin match tmono with
		| None -> ()
		| Some (WithType t2) -> unify ctx t2 t p
		| Some (WithTypeResume t2) -> (try unify_raise ctx t2 t p with Error (Unify l,p) -> raise (Typer.WithTypeError (l,p)))
		| _ -> assert false
	end;
	(* count usage *)
	let usage = Array.make (DynArray.length mctx.dt_lut) 0 in
	(* we always want to keep the first part *)
	let first = (match dt with Goto i -> i | _ -> Hashtbl.find mctx.dt_cache dt) in
	Array.set usage first 2;
	let rec loop dt = match dt with
		| Goto i -> Array.set usage i ((Array.get usage i) + 1)
		| Switch(st,cl) -> List.iter (fun (_,dt) -> loop dt) cl
		| Bind(bl,dt) -> loop dt
		| Expr e -> ()
		| Guard(e,dt1,dt2) ->
			loop dt1;
			match dt2 with None -> () | Some dt -> (loop dt)
	in
	DynArray.iter loop mctx.dt_lut;
	(* filter parts that will be inlined and keep a map to them*)
	let map = Array.make (DynArray.length mctx.dt_lut) 0 in
	let lut = DynArray.create() in
	let rec loop i c =
		if c < DynArray.length mctx.dt_lut then begin
			let i' = if usage.(c) > 1 then begin
				DynArray.add lut (DynArray.get mctx.dt_lut c);
				i + 1
			end else i in
			Array.set map c i;
			loop i' (c + 1)
		end
	in
	loop 0 0;
	(* reindex *)
	let rec loop dt = match dt with
		| Goto i -> if usage.(i) > 1 then DTGoto (map.(i)) else loop (DynArray.get mctx.dt_lut i)
		| Switch(st,cl) -> convert_switch mctx st cl loop
		| Bind(bl,dt) -> DTBind(List.map (fun (v,st) -> v,convert_st ctx st) bl,loop dt)
		| Expr id -> DTExpr (get_expr mctx id)
		| Guard(id,dt1,dt2) -> DTGuard((match get_guard mctx id with Some e -> e | None -> assert false),loop dt1, match dt2 with None -> None | Some dt -> Some (loop dt))
	in
	let lut = DynArray.map loop lut in
	{
		dt_first = map.(first);
		dt_dt_lookup = DynArray.to_array lut;
		dt_type = t;
		dt_var_init = List.rev !var_inits;
		dt_is_complex = !is_complex;
	}
;;
match_expr_ref := match_expr;
get_pattern_locals_ref := get_pattern_locals