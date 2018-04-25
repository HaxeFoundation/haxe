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
open Ast
open Globals
open Error
open Typecore
open Type

let get_submodule_fields ctx path =
	let m = Hashtbl.find ctx.g.modules path in
	let tl = List.filter (fun t -> path <> (t_infos t).mt_path && not (t_infos t).mt_private) m.m_types in
	let tl = List.map (fun mt ->
		let infos = t_infos mt in
		(snd infos.mt_path),Display.FKType (type_of_module_type mt),infos.mt_doc
	) tl in
	tl

let collect ctx e_ast e with_type p =
	let merge_core_doc = !merge_core_doc_ref in
	let opt_args args ret = TFun(List.map(fun (n,o,t) -> n,true,t) args,ret) in
	let e = match e.eexpr with
		| TField (e1,fa) when field_name fa = "bind" ->
			(match follow e1.etype with
			| TFun(args,ret) -> {e1 with etype = opt_args args ret}
			| _ -> e)
		| _ ->
			e
	in
	let opt_type t =
		match t with
		| TLazy f ->
			return_partial_type := true;
			let t = lazy_type f in
			return_partial_type := false;
			t
		| _ ->
			t
	in
	let should_access c cf stat =
		if c != ctx.curclass && not cf.cf_public && String.length cf.cf_name > 4 then begin match String.sub cf.cf_name 0 4 with
			| "get_" | "set_" -> false
			| _ -> can_access ctx c cf stat
		end else
			can_access ctx c cf stat
	in
	let rec get_fields t =
		match follow t with
		| TInst (c,params) ->
			if Meta.has Meta.CoreApi c.cl_meta then merge_core_doc ctx c;
			let merge ?(cond=(fun _ -> true)) a b =
				PMap.foldi (fun k f m -> if cond f then PMap.add k f m else m) a b
			in
			let rec loop c params =
				let m = List.fold_left (fun m (i,params) ->
					merge m (loop i params)
				) PMap.empty c.cl_implements in
				let m = (match c.cl_super with
					| None -> m
					| Some (csup,cparams) -> merge m (loop csup cparams)
				) in
				let m = merge ~cond:(fun f -> should_access c f false) c.cl_fields m in
				let m = (match c.cl_kind with
					| KTypeParameter pl -> List.fold_left (fun acc t -> merge acc (get_fields t)) m pl
					| _ -> m
				) in
				PMap.map (fun f -> { f with cf_type = apply_params c.cl_params params (opt_type f.cf_type); cf_public = true; }) m
			in
			loop c params
		| TAbstract({a_impl = Some c} as a,pl) ->
			if Meta.has Meta.CoreApi c.cl_meta then merge_core_doc ctx c;
			let fields = try
				let _,el,_ = Meta.get Meta.Forward a.a_meta in
				let sl = ExtList.List.filter_map (fun e -> match fst e with
					| EConst(Ident s) -> Some s
					| _ -> None
				) el in
				let fields = get_fields (apply_params a.a_params pl a.a_this) in
				if sl = [] then fields else PMap.fold (fun cf acc ->
					if List.mem cf.cf_name sl then
						PMap.add cf.cf_name cf acc
					else
						acc
				) fields PMap.empty
			with Not_found ->
				PMap.empty
			in
			PMap.fold (fun f acc ->
				if f.cf_name <> "_new" && should_access c f true && Meta.has Meta.Impl f.cf_meta && not (Meta.has Meta.Enum f.cf_meta) then begin
					let f = prepare_using_field f in
					let t = apply_params a.a_params pl (follow f.cf_type) in
					PMap.add f.cf_name { f with cf_public = true; cf_type = opt_type t } acc
				end else
					acc
			) c.cl_statics fields
		| TAnon a when PMap.is_empty a.a_fields ->
			begin match with_type with
			| WithType t -> get_fields t
			| _ -> a.a_fields
			end
		| TAnon a ->
			(match !(a.a_status) with
			| Statics c ->
				if Meta.has Meta.CoreApi c.cl_meta then merge_core_doc ctx c;
				let is_abstract_impl = match c.cl_kind with KAbstractImpl _ -> true | _ -> false in
				let pm = match c.cl_constructor with None -> PMap.empty | Some cf -> PMap.add "new" cf PMap.empty in
				PMap.fold (fun f acc ->
					if should_access c f true && (not is_abstract_impl || not (Meta.has Meta.Impl f.cf_meta) || Meta.has Meta.Enum f.cf_meta) then
						PMap.add f.cf_name { f with cf_public = true; cf_type = opt_type f.cf_type } acc else acc
				) a.a_fields pm
			| _ ->
				a.a_fields)
		| TFun (args,ret) ->
			let t = opt_args args ret in
			let cf = mk_field "bind" (tfun [t] t) p null_pos in
			cf.cf_kind <- Method MethNormal;
			PMap.add "bind" cf PMap.empty
		| _ ->
			PMap.empty
	in
	let fields = get_fields e.etype in
	(*
		add 'using' methods compatible with this type
	*)
	let rec loop acc = function
		| [] -> acc
		| (c,_) :: l ->
			let acc = ref (loop acc l) in
			let rec dup t = Type.map dup t in
			List.iter (fun f ->
				if not (Meta.has Meta.NoUsing f.cf_meta) && not (Meta.has Meta.Impl f.cf_meta) then
				let f = { f with cf_type = opt_type f.cf_type } in
				let monos = List.map (fun _ -> mk_mono()) f.cf_params in
				let map = apply_params f.cf_params monos in
				match follow (map f.cf_type) with
				| TFun((_,_,TType({t_path=["haxe";"macro"], "ExprOf"}, [t])) :: args, ret)
				| TFun((_,_,t) :: args, ret) ->
					(try
						unify_raise ctx (dup e.etype) t e.epos;
						List.iter2 (fun m (name,t) -> match follow t with
							| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
								List.iter (fun tc -> unify_raise ctx m (map tc) e.epos) constr
							| _ -> ()
						) monos f.cf_params;
						if not (can_access ctx c f true) || follow e.etype == t_dynamic && follow t != t_dynamic then
							()
						else begin
							let f = prepare_using_field f in
							let f = { f with cf_params = []; cf_public = true; cf_type = TFun(args,ret) } in
							acc := PMap.add f.cf_name f (!acc)
						end
					with Error (Unify _,_) -> ())
				| _ -> ()
			) c.cl_ordered_statics;
			!acc
	in
	let use_methods = match follow e.etype with TMono _ -> PMap.empty | _ -> loop (loop PMap.empty ctx.g.global_using) ctx.m.module_using in
	let fields = PMap.fold (fun f acc -> PMap.add f.cf_name f acc) fields use_methods in
	let fields = match fst e_ast with
		| EConst(String s) when String.length s = 1 ->
			let cf = mk_field "code" ctx.t.tint e.epos null_pos in
			cf.cf_doc <- Some "The character code of this character (inlined at compile-time).";
			cf.cf_kind <- Var { v_read = AccNormal; v_write = AccNever };
			PMap.add cf.cf_name cf fields
		| _ ->
			fields
	in
	let fields = PMap.fold (fun f acc -> if Meta.has Meta.NoCompletion f.cf_meta then acc else f :: acc) fields [] in
	let get_field acc f =
		List.fold_left (fun acc f ->
			let kind = match f.cf_kind with Method _ -> Display.FKMethod f.cf_type | Var _ -> Display.FKVar f.cf_type in
			if f.cf_public then (f.cf_name,kind,f.cf_doc) :: acc else acc
		) acc (f :: f.cf_overloads)
	in
	let fields = List.fold_left get_field [] fields in
	try
		let sl = string_list_of_expr_path_raise e_ast in
		fields @ get_submodule_fields ctx (List.tl sl,List.hd sl)
	with Exit | Not_found ->
		fields
