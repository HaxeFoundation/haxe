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

(* Various checks performed while loading types. *)

open Globals
open Ast
open Type
open Typecore
open DisplayException
open DisplayTypes
open DisplayMode
open CompletionItem
open CompletionModuleKind
open CompletionModuleType
open CompletionResultKind
open Common
open Error

exception Build_canceled of build_state

let is_generic_parameter ctx c =
	(* first check field parameters, then class parameters *)
	try
		ignore (List.assoc (snd c.cl_path) ctx.curfield.cf_params);
		Meta.has Meta.Generic ctx.curfield.cf_meta
	with Not_found -> try
		ignore(List.assoc (snd c.cl_path) ctx.type_params);
		(match ctx.curclass.cl_kind with | KGeneric -> true | _ -> false);
	with Not_found ->
		false

let valid_redefinition ctx f1 t1 f2 t2 = (* child, parent *)
	let valid t1 t2 =
		Type.unify t1 t2;
		if is_null t1 <> is_null t2 || ((follow t1) == t_dynamic && (follow t2) != t_dynamic) then raise (Unify_error [Cannot_unify (t1,t2)]);
	in
	let open OptimizerTexpr in
	begin match PurityState.get_purity_from_meta f2.cf_meta,PurityState.get_purity_from_meta f1.cf_meta with
		| PurityState.Pure,PurityState.MaybePure -> f1.cf_meta <- (Meta.Pure,[EConst(Ident "expect"),f2.cf_pos],null_pos) :: f1.cf_meta
		| PurityState.ExpectPure p,PurityState.MaybePure -> f1.cf_meta <- (Meta.Pure,[EConst(Ident "expect"),p],null_pos) :: f1.cf_meta
		| _ -> ()
	end;
	let t1, t2 = (match f1.cf_params, f2.cf_params with
		| [], [] -> t1, t2
		| l1, l2 when List.length l1 = List.length l2 ->
			let to_check = ref [] in
			let monos = List.map2 (fun (name,p1) (_,p2) ->
				(match follow p1, follow p2 with
				| TInst ({ cl_kind = KTypeParameter ct1 } as c1,pl1), TInst ({ cl_kind = KTypeParameter ct2 } as c2,pl2) ->
					(match ct1, ct2 with
					| [], [] -> ()
					| _, _ when List.length ct1 = List.length ct2 ->
						(* if same constraints, they are the same type *)
						let check monos =
							List.iter2 (fun t1 t2  ->
								try
									let t1 = apply_params l1 monos (apply_params c1.cl_params pl1 t1) in
									let t2 = apply_params l2 monos (apply_params c2.cl_params pl2 t2) in
									type_eq EqStrict t1 t2
								with Unify_error l ->
									raise (Unify_error (Unify_custom "Constraints differ" :: l))
							) ct1 ct2
						in
						to_check := check :: !to_check;
					| _ ->
						raise (Unify_error [Unify_custom "Different number of constraints"]))
				| _ -> ());
				TInst (mk_class null_module ([],name) null_pos null_pos,[])
			) l1 l2 in
			List.iter (fun f -> f monos) !to_check;
			apply_params l1 monos t1, apply_params l2 monos t2
		| _  ->
			(* ignore type params, will create other errors later *)
			t1, t2
	) in
	match f1.cf_kind,f2.cf_kind with
	| Method m1, Method m2 when not (m1 = MethDynamic) && not (m2 = MethDynamic) ->
		begin match follow t1, follow t2 with
		| TFun (args1,r1) , TFun (args2,r2) -> (
			if not (List.length args1 = List.length args2) then raise (Unify_error [Unify_custom "Different number of function arguments"]);
			let i = ref 0 in
			try
				valid r1 r2;
				List.iter2 (fun (n,o1,a1) (_,o2,a2) ->
					incr i;
					if o1 <> o2 then raise (Unify_error [Not_matching_optional n]);
					(try valid a2 a1 with Unify_error _ -> raise (Unify_error [Cannot_unify(a1,a2)]))
				) args1 args2;
			with Unify_error l ->
				let msg = if !i = 0 then Invalid_return_type else Invalid_function_argument(!i,List.length args1) in
				raise (Unify_error (Cannot_unify (t1,t2) :: msg :: l)))
		| _ ->
			die "" __LOC__
		end
	| _,(Var { v_write = AccNo | AccNever }) ->
		(* write variance *)
		valid t1 t2
	| _,(Var { v_read = AccNo | AccNever }) ->
		(* read variance *)
		valid t2 t1
	| _,_ when has_class_field_flag f2 CfFinal ->
		(* write variance *)
		valid t1 t2
	| _ , _ ->
		(* in case args differs, or if an interface var *)
		type_eq EqStrict t1 t2;
		if is_null t1 <> is_null t2 then raise (Unify_error [Cannot_unify (t1,t2)])

let copy_meta meta_src meta_target sl =
	let meta = ref meta_target in
	List.iter (fun (m,e,p) ->
		if List.mem m sl then meta := (m,e,p) :: !meta
	) meta_src;
	!meta

(** retrieve string from @:native metadata or raise Not_found *)
let get_native_name meta =
	let rec get_native meta = match meta with
		| [] -> raise Not_found
		| (Meta.Native,[v],p as meta) :: _ ->
			meta
		| _ :: meta ->
			get_native meta
	in
	let (_,e,mp) = get_native meta in
	match e with
	| [Ast.EConst (Ast.String(name,_)),p] ->
		name,p
	| [] ->
		raise Not_found
	| _ ->
		error "String expected" mp

let check_native_name_override ctx child base =
	let error base_pos child_pos =
		display_error ctx ("Field " ^ child.cf_name ^ " has different @:native value than in superclass") child_pos;
		display_error ctx ("Base field is defined here") base_pos
	in
	try
		let child_name, child_pos = get_native_name child.cf_meta in
		try
			let base_name, base_pos = get_native_name base.cf_meta in
			if base_name <> child_name then
				error base_pos child_pos
		with Not_found ->
			error base.cf_name_pos child_pos
	with Not_found -> ()

let check_overriding ctx c f =
	match c.cl_super with
	| None ->
		if List.memq f c.cl_overrides then display_error ctx ("Field " ^ f.cf_name ^ " is declared 'override' but doesn't override any field") f.cf_pos
	| _ when c.cl_extern && Meta.has Meta.CsNative c.cl_meta -> () (* -net-lib specific: do not check overrides on extern CsNative classes *)
	| Some (csup,params) ->
		let p = f.cf_name_pos in
		let i = f.cf_name in
		let check_field f get_super_field is_overload = try
			(if is_overload && not (Meta.has Meta.Overload f.cf_meta) then
				display_error ctx ("Missing @:overload declaration for field " ^ i) p);
			let t, f2 = get_super_field csup i in
			check_native_name_override ctx f f2;
			(* allow to define fields that are not defined for this platform version in superclass *)
			(match f2.cf_kind with
			| Var { v_read = AccRequire _ } -> raise Not_found;
			| _ -> ());
			if ctx.com.config.pf_overload && (Meta.has Meta.Overload f2.cf_meta && not (Meta.has Meta.Overload f.cf_meta)) then
				display_error ctx ("Field " ^ i ^ " should be declared with @:overload since it was already declared as @:overload in superclass") p
			else if not (List.memq f c.cl_overrides) then
				display_error ctx ("Field " ^ i ^ " should be declared with 'override' since it is inherited from superclass " ^ s_type_path csup.cl_path) p
			else if not (has_class_field_flag f CfPublic) && (has_class_field_flag f2 CfPublic) then
				display_error ctx ("Field " ^ i ^ " has less visibility (public/private) than superclass one") p
			else (match f.cf_kind, f2.cf_kind with
			| _, Method MethInline ->
				display_error ctx ("Field " ^ i ^ " is inlined and cannot be overridden") p
			| a, b when a = b -> ()
			| Method MethInline, Method MethNormal ->
				() (* allow to redefine a method as inlined *)
			| _ ->
				display_error ctx ("Field " ^ i ^ " has different property access than in superclass") p);
			if (has_class_field_flag f2 CfFinal) then display_error ctx ("Cannot override final method " ^ i) p;
			try
				let t = apply_params csup.cl_params params t in
				valid_redefinition ctx f f.cf_type f2 t;
			with
				Unify_error l ->
					display_error ctx ("Field " ^ i ^ " overrides parent class with different or incomplete type") p;
					display_error ctx ("Base field is defined here") f2.cf_name_pos;
					display_error ctx (error_msg (Unify l)) p;
		with
			Not_found ->
				if List.memq f c.cl_overrides then
					let msg = if is_overload then
						("Field " ^ i ^ " is declared 'override' but no compatible overload was found")
					else begin
						let fields = TClass.get_all_super_fields c in
						let fields = PMap.fold (fun (_,cf) acc -> match cf.cf_kind with
							| Method MethNormal when not (has_class_field_flag cf CfFinal) -> cf.cf_name :: acc
							| _ -> acc
						) fields [] in
						StringError.string_error i fields ("Field " ^ i ^ " is declared 'override' but doesn't override any field")
					end in
					display_error ctx msg p
		in
		if ctx.com.config.pf_overload && Meta.has Meta.Overload f.cf_meta then begin
			let overloads = Overloads.get_overloads csup i in
			List.iter (fun (t,f2) ->
				(* check if any super class fields are vars *)
				match f2.cf_kind with
				| Var _ ->
					display_error ctx ("A variable named '" ^ f2.cf_name ^ "' was already declared in a superclass") f.cf_pos
				| _ -> ()
			) overloads;
			List.iter (fun f ->
				(* find the exact field being overridden *)
				check_field f (fun csup i ->
					List.find (fun (t,f2) ->
						Overloads.same_overload_args f.cf_type (apply_params csup.cl_params params t) f f2
					) overloads
				) true
			) (f :: f.cf_overloads)
		end else
			check_field f (fun csup i ->
				let _, t, f2 = raw_class_field (fun f -> f.cf_type) csup params i in
				t, f2) false

let class_field_no_interf c i =
	try
		let f = PMap.find i c.cl_fields in
		f.cf_type , f
	with Not_found ->
		match c.cl_super with
		| None ->
			raise Not_found
		| Some (c,tl) ->
			(* rec over class_field *)
			let _, t , f = raw_class_field (fun f -> f.cf_type) c tl i in
			apply_params c.cl_params tl t , f

let rec return_flow ctx e =
	let error() =
		display_error ctx (Printf.sprintf "Missing return: %s" (s_type (print_context()) ctx.ret)) e.epos; raise Exit
	in
	let return_flow = return_flow ctx in
	let rec uncond e = match e.eexpr with
		| TIf _ | TWhile _ | TSwitch _ | TTry _ | TFunction _ -> ()
		| TReturn _ | TThrow _ -> raise Exit
		| _ -> Type.iter uncond e
	in
	let has_unconditional_flow e = try uncond e; false with Exit -> true in
	match e.eexpr with
	| TReturn _ | TThrow _ -> ()
	| TParenthesis e | TMeta(_,e) ->
		return_flow e
	| TBlock el ->
		let rec loop = function
			| [] -> error()
			| [e] -> return_flow e
			| e :: _ when has_unconditional_flow e -> ()
			| _ :: l -> loop l
		in
		loop el
	| TIf (_,e1,Some e2) ->
		return_flow e1;
		return_flow e2;
	| TSwitch (v,cases,Some e) ->
		List.iter (fun (_,e) -> return_flow e) cases;
		return_flow e
	| TSwitch ({eexpr = TMeta((Meta.Exhaustive,_,_),_)},cases,None) ->
		List.iter (fun (_,e) -> return_flow e) cases;
	| TTry (e,cases) ->
		return_flow e;
		List.iter (fun (_,e) -> return_flow e) cases;
	| TWhile({eexpr = (TConst (TBool true))},e,_) ->
		(* a special case for "inifite" while loops that have no break *)
		let rec loop e = match e.eexpr with
			(* ignore nested loops to not accidentally get one of its breaks *)
			| TWhile _ | TFor _ -> ()
			| TBreak -> error()
			| _ -> Type.iter loop e
		in
		loop e
	| _ ->
		error()

let check_global_metadata ctx meta f_add mpath tpath so =
	let sl1 = full_dot_path2 mpath tpath in
	let sl1,field_mode = match so with None -> sl1,false | Some s -> sl1 @ [s],true in
	List.iter (fun (sl2,m,(recursive,to_types,to_fields)) ->
		let add = ((field_mode && to_fields) || (not field_mode && to_types)) && (match_path recursive sl1 sl2) in
		if add then f_add m
	) ctx.g.global_metadata;
	if ctx.is_display_file then delay ctx PCheckConstraint (fun () -> DisplayEmitter.check_display_metadata ctx meta)

let check_module_types ctx m p t =
	let t = t_infos t in
	try
		let m2 = Hashtbl.find ctx.g.types_module t.mt_path in
		if m.m_path <> m2 && String.lowercase (s_type_path m2) = String.lowercase (s_type_path m.m_path) then error ("Module " ^ s_type_path m2 ^ " is loaded with a different case than " ^ s_type_path m.m_path) p;
		error ("Type name " ^ s_type_path t.mt_path ^ " is redefined from module " ^ s_type_path m2) p
	with
		Not_found ->
			Hashtbl.add ctx.g.types_module t.mt_path m.m_path

module Inheritance = struct
	let is_basic_class_path path = match path with
		| ([],("Array" | "String" | "Date" | "Xml")) -> true
		| _ -> false

	let check_extends ctx c t p = match follow t with
		| TInst (csup,params) ->
			if is_basic_class_path csup.cl_path && not (c.cl_extern && csup.cl_extern) then error "Cannot extend basic class" p;
			if extends csup c then error "Recursive class" p;
			begin match csup.cl_kind with
				| KTypeParameter _ ->
					if is_generic_parameter ctx csup then error "Extending generic type parameters is no longer allowed in Haxe 4" p;
					error "Cannot extend type parameters" p
				| _ -> csup,params
			end
		| _ -> error "Should extend by using a class" p

	let rec check_interface ctx c intf params =
		let p = c.cl_name_pos in
		let rec check_field i f =
			(if ctx.com.config.pf_overload then
				List.iter (function
					| f2 when f != f2 ->
							check_field i f2
					| _ -> ()) f.cf_overloads);
			let is_overload = ref false in
			try
				let t2, f2 = class_field_no_interf c i in
				let t2, f2 =
					if ctx.com.config.pf_overload && (f2.cf_overloads <> [] || Meta.has Meta.Overload f2.cf_meta) then
						let overloads = Overloads.get_overloads c i in
						is_overload := true;
						let t = (apply_params intf.cl_params params f.cf_type) in
						List.find (fun (t1,f1) -> Overloads.same_overload_args t t1 f f1) overloads
					else
						t2, f2
				in
				ignore(follow f2.cf_type); (* force evaluation *)
				let p = f2.cf_name_pos in
				let mkind = function
					| MethNormal | MethInline -> 0
					| MethDynamic -> 1
					| MethMacro -> 2
				in
				if (has_class_field_flag f CfPublic) && not (has_class_field_flag f2 CfPublic) && not (Meta.has Meta.CompilerGenerated f.cf_meta) then
					display_error ctx ("Field " ^ i ^ " should be public as requested by " ^ s_type_path intf.cl_path) p
				else if not (unify_kind f2.cf_kind f.cf_kind) || not (match f.cf_kind, f2.cf_kind with Var _ , Var _ -> true | Method m1, Method m2 -> mkind m1 = mkind m2 | _ -> false) then
					display_error ctx ("Field " ^ i ^ " has different property access than in " ^ s_type_path intf.cl_path ^ " (" ^ s_kind f2.cf_kind ^ " should be " ^ s_kind f.cf_kind ^ ")") p
				else try
					valid_redefinition ctx f2 t2 f (apply_params intf.cl_params params f.cf_type)
				with
					Unify_error l ->
						if not (Meta.has Meta.CsNative c.cl_meta && c.cl_extern) then begin
							display_error ctx ("Field " ^ i ^ " has different type than in " ^ s_type_path intf.cl_path) p;
							display_error ctx ("Interface field is defined here") f.cf_pos;
							display_error ctx (error_msg (Unify l)) p;
						end
			with
				| Not_found when not c.cl_interface ->
					let msg = if !is_overload then
						let ctx = print_context() in
						let args = match follow f.cf_type with | TFun(args,_) -> String.concat ", " (List.map (fun (n,o,t) -> (if o then "?" else "") ^ n ^ " : " ^ (s_type ctx t)) args) | _ -> die "" __LOC__ in
						"No suitable overload for " ^ i ^ "( " ^ args ^ " ), as needed by " ^ s_type_path intf.cl_path ^ " was found"
					else
						("Field " ^ i ^ " needed by " ^ s_type_path intf.cl_path ^ " is missing")
					in
					display_error ctx msg p
				| Not_found -> ()
		in
		PMap.iter check_field intf.cl_fields;
		List.iter (fun (i2,p2) ->
			check_interface ctx c i2 (List.map (apply_params intf.cl_params params) p2)
		) intf.cl_implements

	let check_interfaces ctx c =
		match c.cl_path with
		| "Proxy" :: _ , _ -> ()
		| _ when c.cl_extern && Meta.has Meta.CsNative c.cl_meta -> ()
		| _ ->
		List.iter (fun (intf,params) -> check_interface ctx c intf params) c.cl_implements

	let set_heritance ctx c herits p =
		let is_lib = Meta.has Meta.LibType c.cl_meta in
		let ctx = { ctx with curclass = c; type_params = c.cl_params; } in
		let old_meta = c.cl_meta in
		let process_meta csup =
			List.iter (fun m ->
				match m with
				| Meta.AutoBuild, el, p -> c.cl_meta <- (Meta.Build,el,{ c.cl_pos with pmax = c.cl_pos.pmin }(* prevent display metadata *)) :: m :: c.cl_meta
				| _ -> ()
			) csup.cl_meta;
			if csup.cl_final && not ((csup.cl_extern && Meta.has Meta.Hack c.cl_meta) || (match c.cl_kind with KTypeParameter _ -> true | _ -> false)) then
				error ("Cannot extend a final " ^ if c.cl_interface then "interface" else "class") p;
		in
		let check_cancel_build csup =
			match csup.cl_build() with
			| Built -> ()
			| state ->
				(* for macros reason, our super class is not yet built - see #2177 *)
				(* let's reset our build and delay it until we are done *)
				c.cl_meta <- old_meta;
				raise (Build_canceled state)
		in
		let has_interf = ref false in
		(*
			resolve imports before calling build_inheritance, since it requires full paths.
			that means that typedefs are not working, but that's a fair limitation
		*)
		let resolve_imports (t,p) =
			match t.tpackage with
			| _ :: _ -> t,p
			| [] ->
				try
					let path_matches lt = snd (t_path lt) = t.tname in
					let lt = try
						List.find path_matches ctx.m.curmod.m_types
					with Not_found ->
						let t,pi = List.find (fun (lt,_) -> path_matches lt) ctx.m.module_types in
						ImportHandling.mark_import_position ctx pi;
						t
					in
					{ t with tpackage = fst (t_path lt) },p
				with
					Not_found -> t,p
		in
		let herits = ExtList.List.filter_map (function
			| HExtends t -> Some(true,resolve_imports t)
			| HImplements t -> Some(false,resolve_imports t)
			| t -> None
		) herits in
		let herits = List.filter (ctx.g.do_inherit ctx c p) herits in
		(* Pass 1: Check and set relations *)
		let check_herit t is_extends p =
			if is_extends then begin
				if c.cl_super <> None then error "Cannot extend several classes" p;
				let csup,params = check_extends ctx c t p in
				if c.cl_interface then begin
					if not csup.cl_interface then error "Cannot extend by using a class" p;
					c.cl_implements <- (csup,params) :: c.cl_implements;
					if not !has_interf then begin
						if not is_lib then delay ctx PForce (fun() -> check_interfaces ctx c);
						has_interf := true;
					end
				end else begin
					if csup.cl_interface then error "Cannot extend by using an interface" p;
					c.cl_super <- Some (csup,params)
				end;
				(fun () ->
					check_cancel_build csup;
					process_meta csup;
				)
			end else begin match follow t with
				| TInst ({ cl_path = [],"ArrayAccess"; cl_extern = true; },[t]) ->
					if c.cl_array_access <> None then error "Duplicate array access" p;
					c.cl_array_access <- Some t;
					(fun () -> ())
				| TInst (intf,params) ->
					if extends intf c then error "Recursive class" p;
					if c.cl_interface then error "Interfaces cannot implement another interface (use extends instead)" p;
					if not intf.cl_interface then error "You can only implement an interface" p;
					c.cl_implements <- (intf, params) :: c.cl_implements;
					if not !has_interf && not is_lib && not (Meta.has (Meta.Custom "$do_not_check_interf") c.cl_meta) then begin
						delay ctx PForce (fun() -> check_interfaces ctx c);
						has_interf := true;
					end;
					(fun () ->
						check_cancel_build intf;
						process_meta intf;
					)
				| TDynamic t ->
					if c.cl_dynamic <> None then error "Cannot have several dynamics" p;
					if not c.cl_extern then display_error ctx "In haxe 4, implements Dynamic is only supported on externs" p;
					c.cl_dynamic <- Some t;
					(fun () -> ())
				| _ ->
					error "Should implement by using an interface" p
			end
		in
		let fl = ExtList.List.filter_map (fun (is_extends,(ct,p)) ->
			try
				let t = try
					Typeload.load_instance ~allow_display:true ctx (ct,p) false
				with DisplayException(DisplayFields Some({fkind = CRTypeHint} as r)) ->
					(* We don't allow `implements` on interfaces. Just raise fields completion with no fields. *)
					if not is_extends && c.cl_interface then raise_fields [] CRImplements r.fsubject;
					let l = List.filter (fun item -> match item.ci_kind with
						| ITType({kind = Interface} as cm,_) -> (not is_extends || c.cl_interface) && CompletionModuleType.get_path cm <> c.cl_path
						| ITType({kind = Class} as cm,_) ->
							is_extends && not c.cl_interface && CompletionModuleType.get_path cm <> c.cl_path &&
							(not cm.is_final || Meta.has Meta.Hack c.cl_meta) &&
							(not (is_basic_class_path (cm.pack,cm.name)) || (c.cl_extern && cm.is_extern))
						| _ -> false
					) r.fitems in
					raise_fields l (if is_extends then CRExtends else CRImplements) r.fsubject
				in
				Some (check_herit t is_extends p)
			with Error(Module_not_found(([],name)),p) when ctx.com.display.dms_kind <> DMNone ->
				if Diagnostics.is_diagnostics_run p then DisplayToplevel.handle_unresolved_identifier ctx name p true;
				None
		) herits in
		fl
end

let check_final_vars ctx e =
	let final_vars = Hashtbl.create 0 in
	List.iter (fun cf -> match cf.cf_kind with
		| Var _ when (has_class_field_flag cf CfFinal) && cf.cf_expr = None ->
			Hashtbl.add final_vars cf.cf_name cf
		| _ ->
			()
	) ctx.curclass.cl_ordered_fields;
	if Hashtbl.length final_vars > 0 then begin
		let rec find_inits e = match e.eexpr with
			| TBinop(OpAssign,{eexpr = TField({eexpr = TConst TThis},fa)},e2) ->
				Hashtbl.remove final_vars (field_name fa);
				find_inits e2;
			| _ ->
				Type.iter find_inits e
		in
		find_inits e;
		Hashtbl.iter (fun _ cf ->
			display_error ctx ("final field " ^ cf.cf_name ^ " must be initialized immediately or in the constructor") cf.cf_pos;
		) final_vars
	end