(*
 *  This file is part of ilLib
 *  Copyright (c)2004-2013 Haxe Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
open IlMeta;;
open IlData;;
open PeReader;;
open ExtString;;

let rec follow s = match s with
	| SReqModifier (_,s)
	| SOptModifier (_,s) ->
		follow s
	| SPinned s ->
		follow s
	| s -> s

(* tells if a type_def_or_ref is of type `path` *)
let rec is_type path = function
	| TypeDef td ->
		td.td_namespace = fst path && td.td_name = snd path
	| TypeRef tr ->
		tr.tr_namespace = fst path && tr.tr_name = snd path
	| TypeSpec ts -> (match follow ts.ts_signature with
	| SClass c | SValueType c ->
		is_type path c
	| SGenericInst(s,_) -> (match follow s with
		| SClass c | SValueType c ->
			is_type path c
		| _ -> false)
	| _ -> false)
	| _ -> assert false

let rec get_path type_def_or_ref = match type_def_or_ref with
	| TypeDef td -> (match td.td_extra_enclosing with
		| None ->
			td.td_namespace,[], td.td_name
		| Some t2 ->
			let ns, nested = match get_path (TypeDef t2) with
				| ns,nested, name ->
					ns, nested @ [name]
			in
			ns,nested, td.td_name)
	| TypeRef tr -> (match tr.tr_resolution_scope with
		| TypeRef tr2 ->
			let ns, nested = match get_path (TypeRef tr2) with
				| ns,nested, name ->
					ns, nested @ [name]
			in
			ns,nested, tr.tr_name
		| _ ->
			tr.tr_namespace,[],tr.tr_name)
	| TypeSpec ts -> (match follow ts.ts_signature with
	| SClass c | SValueType c ->
		get_path c
	| SGenericInst(s,_) -> (match follow s with
		| SClass c | SValueType c ->
			get_path c
		| _ -> [],[],"")
	| _ -> [],[],"")
	| _ -> assert false

let constant_s = function
	| IBool true -> "true"
	| IBool false -> "false"
	| IChar chr -> "'" ^ Char.escaped (Char.chr chr) ^ "'"
	| IByte i ->
		Printf.sprintf "(byte) 0x%x" i
	| IShort i ->
		Printf.sprintf "(short) 0x%x" i
	| IInt i ->
		Printf.sprintf "0x%lx" i
	| IInt64 i ->
		Printf.sprintf "0x%Lx" i
	| IFloat32 f ->
		Printf.sprintf "%ff" f
	| IFloat64 f ->
		Printf.sprintf "%fd" f
	| IString s -> "\"" ^ s ^ "\""
	| INull -> "null"

let rec instance_s = function
	| InstConstant c -> constant_s c
	| InstBoxed b -> "boxed " ^ instance_s b
	| InstType t -> "Type " ^ t
	| InstArray il -> "[" ^ String.concat ", " (List.map instance_s il) ^ "]"
	| InstEnum e -> "Enum " ^ string_of_int e

let named_attribute_s (is_prop,name,inst) =
	(if is_prop then
		"/*prop*/ "
	else
		"")
	^ name ^ " = " ^ instance_s inst

let attributes_s (il,nal) =
	"(" ^ (String.concat ", " (List.map instance_s il)) ^ (if nal <> [] then ", " ^ (String.concat ", " (List.map named_attribute_s nal)) else "") ^")"

let meta_root m : meta_root = match m with
	| Module r -> Obj.magic r
	| TypeRef r -> Obj.magic r
	| TypeDef r -> Obj.magic r
	| FieldPtr r -> Obj.magic r
	| Field r -> Obj.magic r
	| MethodPtr r -> Obj.magic r
	| Method r -> Obj.magic r
	| ParamPtr r -> Obj.magic r
	| Param r -> Obj.magic r
	| InterfaceImpl r -> Obj.magic r
	| MemberRef r -> Obj.magic r
	| Constant r -> Obj.magic r
	| CustomAttribute r -> Obj.magic r
	| FieldMarshal r -> Obj.magic r
	| DeclSecurity r -> Obj.magic r
	| ClassLayout r -> Obj.magic r
	| FieldLayout r -> Obj.magic r
	| StandAloneSig r -> Obj.magic r
	| EventMap r -> Obj.magic r
	| EventPtr r -> Obj.magic r
	| Event r -> Obj.magic r
	| PropertyMap r -> Obj.magic r
	| PropertyPtr r -> Obj.magic r
	| Property r -> Obj.magic r
	| MethodSemantics r -> Obj.magic r
	| MethodImpl r -> Obj.magic r
	| ModuleRef r -> Obj.magic r
	| TypeSpec r -> Obj.magic r
	| ImplMap r -> Obj.magic r
	| FieldRVA r -> Obj.magic r
	| ENCLog r -> Obj.magic r
	| ENCMap r -> Obj.magic r
	| Assembly r -> Obj.magic r
	| AssemblyProcessor r -> Obj.magic r
	| AssemblyOS r -> Obj.magic r
	| AssemblyRef r -> Obj.magic r
	| AssemblyRefProcessor r -> Obj.magic r
	| AssemblyRefOS r -> Obj.magic r
	| File r -> Obj.magic r
	| ExportedType r -> Obj.magic r
	| ManifestResource r -> Obj.magic r
	| NestedClass r -> Obj.magic r
	| GenericParam r -> Obj.magic r
	| MethodSpec r -> Obj.magic r
	| GenericParamConstraint r -> Obj.magic r
	| _ -> assert false

let meta_root_ptr p : meta_root_ptr = match p with
	| FieldPtr r -> Obj.magic r
	| MethodPtr r -> Obj.magic r
	| ParamPtr r -> Obj.magic r
	| EventPtr r -> Obj.magic r
	| _ -> assert false

let rec ilsig_norm = function
	| SVoid -> LVoid
	| SBool -> LBool
	| SChar -> LChar
	| SInt8 -> LInt8
	| SUInt8 -> LUInt8
	| SInt16 -> LInt16
	| SUInt16 -> LUInt16
	| SInt32 -> LInt32
	| SUInt32 -> LUInt32
	| SInt64 -> LInt64
	| SUInt64 -> LUInt64
	| SFloat32 -> LFloat32
	| SFloat64 -> LFloat64
	| SString -> LString
	| SPointer p -> LPointer (ilsig_norm p)
	| SManagedPointer p -> LManagedPointer (ilsig_norm p)
	| SValueType v -> LValueType (get_path v, [])
	| SClass v -> LClass (get_path v, [])
	| STypeParam i -> LTypeParam i
	| SArray (t, opts) -> LArray(ilsig_norm t, opts)
	| SGenericInst (p,args) -> (match follow p with
		| SClass v ->
			LClass(get_path v, List.map ilsig_norm args)
		| SValueType v ->
			LValueType(get_path v, List.map ilsig_norm args)
		| _ -> assert false)
	| STypedReference -> LTypedReference
	| SIntPtr -> LIntPtr
	| SUIntPtr -> LUIntPtr
	| SFunPtr(conv,ret,args) -> LMethod(conv,ilsig_norm ret,List.map ilsig_norm args)
	| SObject -> LObject
	| SVector s -> LVector (ilsig_norm s)
	| SMethodTypeParam i -> LMethodTypeParam i
	| SReqModifier (_,s) -> ilsig_norm s
	| SOptModifier (_,s) -> ilsig_norm s
	| SSentinel -> LSentinel
	| SPinned s -> ilsig_norm s
	| SType -> LClass( (["System"],[],"Type"), [])
	| SBoxed -> LObject
	| SEnum e ->
		let lst = String.nsplit e "." in
		let rev = List.rev lst in
		match rev with
		| hd :: tl -> LValueType( (List.rev tl,[],hd), [] )
		| _ -> assert false

let ilsig_t s =
	{
		snorm = ilsig_norm s;
		ssig = s;
	}

let ilsig_of_tdef_ref = function
	| TypeDef td ->
		SClass (TypeDef td)
	| TypeRef tr ->
		SClass (TypeRef tr)
	| TypeSpec ts ->
		ts.ts_signature
	| s ->
		(* error ("Invalid tdef_or_ref: " ^ ilsig_s s) *)
		error "Invalid tdef_or_ref"

let convert_field ctx f =
	{
		fname = f.f_name;
		fflags = f.f_flags;
		fsig = ilsig_t f.f_signature;
	}

let convert_generic ctx gp =
	let constraints = List.fold_left (fun c -> function
		| GenericParamConstraint gc ->
			ilsig_t (ilsig_of_tdef_ref gc.gc_constraint) :: c
		| _ ->
			c
	) [] (Hashtbl.find_all ctx.il_relations (IGenericParam, gp.gp_id))
	in
	{
		tnumber = gp.gp_number;
		tflags = gp.gp_flags;
		tname = gp.gp_name;
		tconstraints = constraints;
	}

let convert_method ctx m =
	let msig = ilsig_t m.m_signature in
	let ret, margs = match follow msig.ssig with
	| SFunPtr (_,ret,args) ->
		(* print_endline m.m_name; *)
		(* print_endline (Printf.sprintf "%d vs %d" (List.length args) (List.length m.m_param_list)); *)
		(* print_endline (String.concat ", " (List.map (fun p ->string_of_int p.p_sequence ^ ":" ^ p.p_name) m.m_param_list)); *)
		(* print_endline "\n"; *)
		(* TODO: find out WHY this happens *)
		let param_list = List.filter (fun p -> p.p_sequence > 0) m.m_param_list in
		ilsig_t ret, List.map2 (fun p s ->
			p.p_name, p.p_flags, ilsig_t s
		) param_list args
	| _ -> assert false
	in

	let override, types =
		List.fold_left (fun (override,types) -> function
		| MethodImpl mi ->
			let declaring = match mi.mi_method_declaration with
				| MemberRef mr ->
					Some (get_path mr.memr_class, mr.memr_name)
				| Method m -> (match m.m_declaring with
					| Some td ->
						Some (get_path (TypeDef td), m.m_name)
					| None -> override)
				| _ -> override
			in
			declaring, types
		| GenericParam gp ->
			override, (convert_generic ctx gp) :: types
		| _ ->
			override,types
		) (None,[]) (Hashtbl.find_all ctx.il_relations (IMethod, m.m_id))
	in
	{
		mname = m.m_name;
		mflags = m.m_flags;
		msig = msig;
		margs = margs;
		mret = ret;
		moverride = override;
		mtypes = types;
	}

let convert_prop ctx prop =
	let name = prop.prop_name in
	let flags = prop.prop_flags in
	let psig = ilsig_t prop.prop_type in
	let pget, pset =
		List.fold_left (fun (get,set) -> function
			| MethodSemantics ms when List.mem SGetter ms.ms_semantic ->
				assert (get = None);
				Some (ms.ms_method.m_name, ms.ms_method.m_flags), set
			| MethodSemantics ms when List.mem SSetter ms.ms_semantic ->
				assert (set = None);
				get, Some (ms.ms_method.m_name,ms.ms_method.m_flags)
			| _ -> get,set
		)
		(None,None)
		(Hashtbl.find_all ctx.il_relations (IProperty, prop.prop_id))
	in
	{
		pname = name;
		psig = psig;
		pflags = flags;
		pget = pget;
		pset = pset;
	}

let convert_event ctx event =
	let name = event.e_name in
	let flags = event.e_flags in
	let esig = ilsig_of_tdef_ref event.e_event_type in
	let esig = ilsig_t esig in
	let add, remove, eraise =
		List.fold_left (fun (add, remove, eraise) -> function
			| MethodSemantics ms when List.mem SAddOn ms.ms_semantic ->
				assert (add = None);
				Some (ms.ms_method.m_name, ms.ms_method.m_flags), remove, eraise
			| MethodSemantics ms when List.mem SRemoveOn ms.ms_semantic ->
				assert (remove = None);
				add, Some (ms.ms_method.m_name,ms.ms_method.m_flags), eraise
			| MethodSemantics ms when List.mem SFire ms.ms_semantic ->
				assert (eraise = None);
				add, remove, Some (ms.ms_method.m_name, ms.ms_method.m_flags)
			| _ -> add, remove, eraise
		)
		(None,None,None)
		(Hashtbl.find_all ctx.il_relations (IEvent, event.e_id))
	in
	{
		ename = name;
		eflags = flags;
		esig = esig;
		eadd = add;
		eremove = remove;
		eraise = eraise;
	}

let convert_class ctx path =
	let td = Hashtbl.find ctx.il_typedefs path in
	let cpath = get_path (TypeDef td) in
	let cflags = td.td_flags in
	let csuper = Option.map (fun e -> ilsig_t (ilsig_of_tdef_ref e)) td.td_extends in
	let cfields = List.map (convert_field ctx) td.td_field_list in
	let cmethods = List.map (convert_method ctx) td.td_method_list in
	let enclosing = Option.map (fun t -> get_path (TypeDef t)) td.td_extra_enclosing in
	let impl, types, nested, props, events =
		List.fold_left (fun (impl,types,nested,props,events) -> function
			| InterfaceImpl ii ->
				(ilsig_t (ilsig_of_tdef_ref ii.ii_interface)) :: impl,types,nested, props, events
			| GenericParam gp ->
				(impl, (convert_generic ctx gp) :: types, nested, props,events)
			| NestedClass nc ->
				assert (nc.nc_enclosing.td_id = td.td_id);
				(impl,types,(get_path (TypeDef nc.nc_nested)) :: nested, props,events)
			| PropertyMap pm ->
				assert (props = []);
				impl,types,nested,List.map (convert_prop ctx) pm.pm_property_list,events
			| EventMap em ->
				assert (events = []);
				(impl,types,nested,props,List.map (convert_event ctx) em.em_event_list)
			| _ ->
				(impl,types,nested,props,events)
		)
		([],[],[],[],[])
		(Hashtbl.find_all ctx.il_relations (ITypeDef, td.td_id))
	in
	{
		cpath = cpath;
		cflags = cflags;
		csuper = csuper;
		cfields = cfields;
		cmethods = cmethods;
		cevents = events;
		cprops = props;
		cimplements = impl;
		ctypes = types;
		cenclosing = enclosing;
		cnested = nested;
	}
