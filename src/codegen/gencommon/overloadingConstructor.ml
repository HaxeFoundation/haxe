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
open Option
open Common
open Type
open Codegen
open Gencommon

(* ******************************************* *)
(* overloading reflection constructors *)
(* ******************************************* *)
(*
	this module works on languages that support function overloading and
	enable function hiding via static functions.
	it takes the constructor body out of the constructor and adds it to a special ctor
	static function. The static function will receive the same parameters as the constructor,
	plus the special "me" var, which will replace "this"

	Then it always adds two constructors to the class: one that receives a special marker class,
	indicating that the object should be constructed without executing constructor body,
	and one that executes its normal constructor.
	Both will only include a super() call to the superclasses' emtpy constructor.

	This enables two things:
		empty construction without the need of incompatibility with the platform's native construction method
		the ability to call super() constructor in any place in the constructor
*)

let rec prev_ctor c tl =
	match c.cl_super with
	| None ->
		raise Not_found
	| Some (sup,stl) ->
		let stl = List.map (apply_params c.cl_params tl) stl in
		match sup.cl_constructor with
		| None -> prev_ctor sup stl
		| Some ctor -> ctor, sup, stl

let make_static_ctor_name cl =
	let name = mk_internal_name "hx" "ctor" in
	name ^ "_" ^ (String.concat "_" (fst cl.cl_path)) ^ "_" ^ (snd cl.cl_path)

(* replaces super() call with last static constructor call *)
let replace_super_call com c tl with_params me p follow_type =
	let rec loop_super c tl =
		match c.cl_super with
		| None ->
			raise Not_found
		| Some(sup,stl) ->
			let stl = List.map (apply_params c.cl_params tl) stl in
			try
				let static_ctor_name = make_static_ctor_name sup in
				sup, stl, PMap.find static_ctor_name sup.cl_statics
			with Not_found ->
				loop_super sup stl
	in
	let sup, stl, cf = loop_super c tl in
	let with_params = (mk (TLocal me) me.v_type p) :: with_params in
	let cf =
		try
			(* choose best super function *)
			List.iter (fun e -> replace_mono e.etype) with_params;
			List.find (fun cf ->
				replace_mono cf.cf_type;
				let args, _ = get_fun (apply_params cf.cf_params stl cf.cf_type) in
				try
					List.for_all2 (fun (_,_,t) e -> try
						let e_etype = follow_type e.etype in
						let t = follow_type t in
						unify e_etype t; true
					with Unify_error _ ->
						false
					) args with_params
				with Invalid_argument _ ->
					false
			) (cf :: cf.cf_overloads)
		with Not_found ->
			com.error "No suitable overload for the super call arguments was found" p; cf
	in
	{
		eexpr = TCall(
			{
				eexpr = TField(Texpr.Builder.make_static_this sup p, FStatic(sup,cf));
				etype = apply_params cf.cf_params stl cf.cf_type;
				epos = p
			},
			with_params
		);
		etype = com.basic.tvoid;
		epos = p;
	}

(* will create a static counterpart of 'ctor', and replace its contents to a call to the static version*)
let create_static_ctor com ~empty_ctor_expr cl ctor follow_type =
	match Meta.has Meta.SkipCtor ctor.cf_meta with
	| true -> ()
	| false when is_none ctor.cf_expr -> ()
	| false ->
		let static_ctor_name = make_static_ctor_name cl in
		(* create the static constructor *)
		let ctor_types = List.map (fun (s,t) -> (s, TInst(map_param (get_cl_t t), []))) cl.cl_params in
		let ctor_type_params = List.map snd ctor_types in
		List.iter (function (_,TInst(c,[])) -> (
			match c.cl_kind with
			| KTypeParameter (hd :: tail) ->
				let before = hd :: tail in
				let after = List.map (apply_params cl.cl_params ctor_type_params) (before) in
				c.cl_kind <- KTypeParameter(after)
			| _ -> ())
		| _ -> ()) ctor_types;
		let me = alloc_var "__hx_this" (TInst(cl, List.map snd ctor_types)) in
		add_var_flag me VCaptured;

		let fn_args, _ = get_fun ctor.cf_type in
		let ctor_params = List.map snd ctor_types in
		let fn_type = TFun((me.v_name,false, me.v_type) :: List.map (fun (n,o,t) -> (n,o,apply_params cl.cl_params ctor_params t)) fn_args, com.basic.tvoid) in
		let cur_tf_args = match ctor.cf_expr with
		| Some { eexpr = TFunction(tf) } -> tf.tf_args
		| _ -> Globals.die "" __LOC__
		in

		let changed_tf_args = List.map (fun (v,_) -> (v,None)) cur_tf_args in

		let local_map = Hashtbl.create (List.length cur_tf_args) in
		let static_tf_args = (me, None) :: List.map (fun (v,b) ->
			let new_v = alloc_var v.v_name (apply_params cl.cl_params ctor_params v.v_type) in
			add_var_flag new_v VCaptured;
			Hashtbl.add local_map v.v_id new_v;
			(new_v, b)
		) cur_tf_args in

		let static_ctor = mk_class_field ~static:true static_ctor_name fn_type false ctor.cf_pos (Method MethNormal) ctor_types in
		let static_ctor_meta = if has_class_flag cl CFinal then Meta.Private else Meta.Protected in
		static_ctor.cf_meta <- (static_ctor_meta,[],ctor.cf_pos) :: static_ctor.cf_meta;

		(* change ctor contents to reference the 'me' var instead of 'this' *)
		let actual_super_call = ref None in
		let rec map_expr ~is_first e = match e.eexpr with
			| TCall (({ eexpr = TConst TSuper } as tsuper), params) -> (try
				let params = List.map (fun e -> map_expr ~is_first:false e) params in
				actual_super_call := Some { e with eexpr = TCall(tsuper, [empty_ctor_expr]) };
				replace_super_call com cl ctor_params params me e.epos follow_type
			with | Not_found ->
				(* last static function was not found *)
				actual_super_call := Some e;
				if not is_first then
					com.error "Super call must be the first call when extending native types" e.epos;
				{ e with eexpr = TBlock([]) })
			| TFunction tf when is_first ->
				do_map ~is_first:true e
			| TConst TThis ->
				mk_local me e.epos
			| TBlock (fst :: bl) ->
				let fst = map_expr ~is_first:is_first fst in
				{ e with eexpr = TBlock(fst :: List.map (fun e -> map_expr ~is_first:false e) bl); etype = apply_params cl.cl_params ctor_params e.etype }
			| _ ->
				do_map e
		and do_map ?(is_first=false) e =
			let do_t = apply_params cl.cl_params ctor_params in
			let do_v v = try
					Hashtbl.find local_map v.v_id
				with | Not_found ->
					v.v_type <- do_t v.v_type; v
			in
			Type.map_expr_type (map_expr ~is_first:is_first) do_t do_v e
		in

		let expr = do_map ~is_first:true (get ctor.cf_expr) in
		let expr = match expr.eexpr with
		| TFunction(tf) ->
			{ expr with etype = fn_type; eexpr = TFunction({ tf with tf_args = static_tf_args }) }
		| _ -> Globals.die "" __LOC__ in
		static_ctor.cf_expr <- Some expr;
		(* add to the statics *)
		(try
			let stat = PMap.find static_ctor_name cl.cl_statics in
			stat.cf_overloads <- static_ctor :: stat.cf_overloads
		with | Not_found ->
			cl.cl_ordered_statics <- static_ctor :: cl.cl_ordered_statics;
			cl.cl_statics <- PMap.add static_ctor_name static_ctor cl.cl_statics);
		(* change current super call *)
		match ctor.cf_expr with
		| Some({ eexpr = TFunction(tf) } as e) ->
			let block_contents, p = match !actual_super_call with
			| None -> [], ctor.cf_pos
			| Some super -> [super], super.epos
			in
			let block_contents = block_contents @ [{
				eexpr = TCall(
					{
						eexpr = TField(
							Texpr.Builder.make_static_this cl p,
							FStatic(cl, static_ctor));
						etype = apply_params static_ctor.cf_params (List.map snd cl.cl_params) static_ctor.cf_type;
						epos = p
					},
					[{ eexpr = TConst TThis; etype = TInst(cl, List.map snd cl.cl_params); epos = p }]
					@ List.map (fun (v,_) -> mk_local v p) cur_tf_args
				);
				etype = com.basic.tvoid;
				epos = p
			}] in
			ctor.cf_expr <- Some { e with eexpr = TFunction({ tf with tf_expr = { tf.tf_expr with eexpr = TBlock block_contents }; tf_args = changed_tf_args }) }
		| _ -> Globals.die "" __LOC__

(* makes constructors that only call super() for the 'ctor' argument *)
let clone_ctors com ctor sup stl cl =
	let rec clone cf =
		let ncf = mk_class_field "new" (apply_params sup.cl_params stl cf.cf_type) (has_class_field_flag cf CfPublic) cf.cf_pos cf.cf_kind cf.cf_params in
		if Meta.has Meta.Protected cf.cf_meta then
			ncf.cf_meta <- (Meta.Protected,[],ncf.cf_pos) :: ncf.cf_meta;
		let args, ret = get_fun ncf.cf_type in
		(* single expression: call to super() *)
		let tf_args = List.map (fun (name,_,t) ->
			(* the constructor will have no optional arguments, as presumably this will be handled by the underlying expr *)
			alloc_var name t, None
		) args in
		let super_call =
		{
			eexpr = TCall(
				{ eexpr = TConst TSuper; etype = TInst(cl, List.map snd cl.cl_params); epos = ctor.cf_pos },
				List.map (fun (v,_) -> mk_local v ctor.cf_pos) tf_args);
			etype = com.basic.tvoid;
			epos = ctor.cf_pos;
		} in
		ncf.cf_expr <- Some
		{
			eexpr = TFunction {
				tf_args = tf_args;
				tf_type = com.basic.tvoid;
				tf_expr = mk_block super_call;
			};
			etype = ncf.cf_type;
			epos = ctor.cf_pos;
		};
		ncf
	in
	(* take off createEmpty *)
	let all = List.filter (fun cf -> replace_mono cf.cf_type; not (Meta.has Meta.SkipCtor cf.cf_meta)) (ctor :: ctor.cf_overloads) in
	let clones = List.map clone all in
	match clones with
	| [] ->
		(* raise Not_found *)
		Globals.die "" __LOC__ (* should never happen *)
	| cf :: [] -> cf
	| cf :: overl ->
		cf.cf_meta <- (Meta.Overload,[],cf.cf_pos) :: cf.cf_meta;
		cf.cf_overloads <- overl; cf

let rec descends_from_native_or_skipctor cl =
	not (is_hxgen (TClassDecl cl)) || Meta.has Meta.SkipCtor cl.cl_meta || match cl.cl_super with
	| None -> false
	| Some(c,_) -> descends_from_native_or_skipctor c

let ensure_super_is_first com cf =
	let rec loop e =
		match e.eexpr with
		| TBlock (b :: block) ->
			loop b
		| TBlock []
		| TCall({ eexpr = TConst TSuper },_) -> ()
		| _ ->
			com.error "Types that derive from a native class must have its super() call as the first statement in the constructor" cf.cf_pos
	in
	match cf.cf_expr with
	| None -> ()
	| Some e -> Type.iter loop e

let init com (empty_ctor_type : t) (empty_ctor_expr : texpr) (follow_type : t -> t) =
	let basic = com.basic in
	let should_change cl = not (has_class_flag cl CInterface) && (not cl.cl_extern || is_hxgen (TClassDecl cl)) && (match cl.cl_kind with KAbstractImpl _ | KModuleFields _ -> false | _ -> true) in
	let msize = List.length com.types in
	let processed, empty_ctors = Hashtbl.create msize, Hashtbl.create msize in

	let rec get_last_empty cl =
		try
			Hashtbl.find empty_ctors cl.cl_path
		with | Not_found ->
			match cl.cl_super with
			| None -> raise Not_found
			| Some (sup,_) -> get_last_empty sup
	in

	let rec change cl =
		if not (Hashtbl.mem processed cl.cl_path) then begin
			Hashtbl.add processed cl.cl_path true;

			(* make sure we've processed the super types *)
			Option.may (fun (super,_) -> if should_change super then change super) cl.cl_super;

			(* implement static hx_ctor and reimplement constructors *)
			(try
				let ctor =
					match cl.cl_constructor with
					| Some ctor ->
						ctor
					| None ->
						try
							let sctor, sup, stl = prev_ctor cl (List.map snd cl.cl_params) in
							(* we'll make constructors that will only call super() *)
							let ctor = clone_ctors com sctor sup stl cl in
							cl.cl_constructor <- Some ctor;
							ctor
						with Not_found -> (* create default constructor *)
							let ctor = mk_class_field "new" (TFun ([], basic.tvoid)) false cl.cl_pos (Method MethNormal) [] in
							ctor.cf_expr <- Some {
								eexpr = TFunction {
									tf_args = [];
									tf_type = basic.tvoid;
									tf_expr = mk (TBlock []) basic.tvoid cl.cl_pos;
								};
								etype = ctor.cf_type;
								epos = ctor.cf_pos;
							};
							cl.cl_constructor <- Some ctor;
							ctor
				in

				let has_super_constructor =
					match cl.cl_super with
						| None -> false
						| Some (csup,_) -> has_constructor csup
				in

				(* now that we made sure we have a constructor, exit if native gen *)
				if not (is_hxgen (TClassDecl cl)) || Meta.has Meta.SkipCtor cl.cl_meta then begin
					if descends_from_native_or_skipctor cl && has_super_constructor then
						List.iter (fun cf -> ensure_super_is_first com cf) (ctor :: ctor.cf_overloads);
					raise Exit
				end;

				(* if cl descends from a native class, we cannot use the static constructor strategy *)
				if descends_from_native_or_skipctor cl && has_super_constructor then
					List.iter (fun cf -> ensure_super_is_first com cf) (ctor :: ctor.cf_overloads)
				else
					(* now that we have a current ctor, create the static counterparts *)
					List.iter (fun cf -> create_static_ctor com ~empty_ctor_expr:empty_ctor_expr cl cf follow_type) (ctor :: ctor.cf_overloads)
			with Exit -> ());

			(* implement empty ctor *)
			(try
				(* now that we made sure we have a constructor, exit if native gen *)
				if not (is_hxgen (TClassDecl cl)) then raise Exit;

				(* get first *)
				let empty_type = TFun (["empty",false,empty_ctor_type],basic.tvoid) in
				let super =
					match cl.cl_super with
					| None -> (* implement empty *)
							[]
					| Some (sup,_) ->
						try
							ignore (get_last_empty sup);
							let esuper = mk (TConst TSuper) (TInst (cl, List.map snd cl.cl_params)) cl.cl_pos in
							[mk (TCall (esuper, [empty_ctor_expr])) basic.tvoid cl.cl_pos]
						with Not_found ->
							try
								(* super type is native: find super constructor with least arguments *)
								let sctor, sup, stl = prev_ctor cl (List.map snd cl.cl_params) in
								let rec loop remaining (best,n) =
									match remaining with
									| [] -> best
									| cf :: r ->
										let args,_ = get_fun cf.cf_type in
										if (List.length args) < n then
											loop r (cf,List.length args)
										else
											loop r (best,n)
								in
								let args,_ = get_fun sctor.cf_type in
								let best = loop sctor.cf_overloads (sctor, List.length args) in
								let args,_ = get_fun (apply_params sup.cl_params stl best.cf_type) in
								let esuper = mk (TConst TSuper) (TInst (sup, stl)) cl.cl_pos in
								[mk (TCall (esuper, List.map (fun (n,o,t) -> null t cl.cl_pos) args)) basic.tvoid cl.cl_pos]
							with Not_found ->
								(* extends native type, but no ctor found *)
								[]
				in
				let ctor = mk_class_field "new" empty_type false cl.cl_pos (Method MethNormal) [] in
				ctor.cf_expr <- Some {
					eexpr = TFunction {
						tf_type = basic.tvoid;
						tf_args = [alloc_var "empty" empty_ctor_type, None];
						tf_expr = mk (TBlock super) basic.tvoid cl.cl_pos
					};
					etype = empty_type;
					epos = cl.cl_pos;
				};
				ctor.cf_meta <- [Meta.SkipCtor, [], ctor.cf_pos];
				Hashtbl.add empty_ctors cl.cl_path ctor;
				match cl.cl_constructor with
				| None ->
					cl.cl_constructor <- Some ctor
				| Some c ->
					c.cf_overloads <- ctor :: c.cf_overloads
			with Exit -> ());
		end
	in

	let module_filter md =
		(match md with
		| TClassDecl cl when should_change cl ->
			change cl;
		| _ ->
			());
		md
	in
	module_filter

let init_expr_filter create_empty =
	let rec run e =
		match e.etype, e.eexpr with
		| TInst (cl, params), TCall ({ eexpr = TField (_, FStatic ({cl_path = [],"Type"}, {cf_name = "createEmptyInstance"})) }, [{eexpr = TTypeExpr ((TClassDecl cl_arg) as mt_arg) }]) when cl == cl_arg && is_hxgen mt_arg ->
			create_empty cl params e.epos
		| _ ->
			Type.map_expr run e
	in
	run

let priority = 0.0
let name = "overloading_constructor"

let configure gen ~empty_ctor_type ~empty_ctor_expr =
	gen.gtools.r_create_empty <- (fun cl params pos -> mk (TNew(cl,params,[empty_ctor_expr])) (TInst(cl,params)) pos);
	let module_filter = init gen.gcon empty_ctor_type empty_ctor_expr (run_follow gen) in
	gen.gmodule_filters#add name (PCustom priority) module_filter;
	let expr_filter = init_expr_filter gen.gtools.r_create_empty in
	gen.gexpr_filters#add name (PCustom priority) expr_filter
