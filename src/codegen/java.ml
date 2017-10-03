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
open Unix
open ExtString
open Common
open Globals
open Ast
open JData

(** Java lib *)

module SS = Set.Make(String)

type java_lib_ctx = {
	jcom : Common.context;
	(* current tparams context *)
	mutable jtparams : jtypes list;
}

exception ConversionError of string * pos

let error s p = raise (ConversionError (s, p))

let is_haxe_keyword = function
	| "cast" | "extern" | "function" | "in" | "typedef" | "using" | "var" | "untyped" | "inline" -> true
	| _ -> false

let jname_to_hx name =
	let name =
		if name <> "" && (String.get name 0 < 'A' || String.get name 0 > 'Z') then
			Char.escaped (Char.uppercase (String.get name 0)) ^ String.sub name 1 (String.length name - 1)
		else
			name
	in
	let name = String.concat "__" (String.nsplit name "_") in
	String.map (function | '$' -> '_' | c -> c) name

let normalize_pack pack =
	List.map (function
		| "" -> ""
		| str when String.get str 0 >= 'A' && String.get str 0 <= 'Z' ->
			String.lowercase str
		| str -> str
	) pack

let jpath_to_hx (pack,name) = match pack, name with
	| ["haxe";"root"], name -> [], name
	| "com" :: ("oracle" | "sun") :: _, _
	| "javax" :: _, _
	| "org" :: ("ietf" | "jcp" | "omg" | "w3c" | "xml") :: _, _
	| "sun" :: _, _
	| "sunw" :: _, _ -> "java" :: normalize_pack pack, jname_to_hx name
	| pack, name -> normalize_pack pack, jname_to_hx name

let real_java_path ctx (pack,name) =
	s_type_path (pack, name)

let lookup_jclass com path =
	let path = jpath_to_hx path in
	List.fold_right (fun (_,_,_,_,get_raw_class) acc ->
		match acc with
		| None -> get_raw_class path
		| Some p -> Some p
	) com.java_libs None

let mk_type_path ctx path params =
	let name, sub = try
		let p, _ = String.split (snd path) "$" in
		jname_to_hx p, Some (jname_to_hx (snd path))
		with | Invalid_string ->
			jname_to_hx (snd path), None
	in
	let pack = fst (jpath_to_hx path) in
	let pack, sub, name = match path with
		| [], ("Float" as c)
		| [], ("Int" as c)
		| [], ("Single" as c)
		| [], ("Bool" as c)
		| [], ("Dynamic" as c)
		| [], ("Iterator" as c)
		| [], ("ArrayAccess" as c)
		| [], ("Iterable" as c) ->
			[], Some c, "StdTypes"
		| [], ("String" as c) ->
			["std"], None, c
		| _ ->
			pack, sub, name
	in
	CTPath {
		tpackage = pack;
		tname = name;
		tparams = params;
		tsub = sub;
	}

let has_tparam name params = List.exists(fun (n,_,_) -> n = name) params

let rec convert_arg ctx p arg =
	match arg with
	| TAny | TType (WSuper, _) -> TPType (mk_type_path ctx ([], "Dynamic") [],null_pos)
	| TType (_, jsig) -> TPType (convert_signature ctx p jsig,null_pos)

and convert_signature ctx p jsig =
	match jsig with
	| TByte -> mk_type_path ctx (["java"; "types"], "Int8") []
	| TChar -> mk_type_path ctx (["java"; "types"], "Char16") []
	| TDouble -> mk_type_path ctx ([], "Float") []
	| TFloat -> mk_type_path ctx ([], "Single") []
	| TInt -> mk_type_path ctx ([], "Int") []
	| TLong -> mk_type_path ctx (["haxe"], "Int64") []
	| TShort -> mk_type_path ctx (["java"; "types"], "Int16") []
	| TBool -> mk_type_path ctx ([], "Bool") []
	| TObject ( (["haxe";"root"], name), args ) -> mk_type_path ctx ([], name) (List.map (convert_arg ctx p) args)
	(** nullable types *)
	(* replaced from Null<Type> to the actual abstract type to fix #2738 *)
	(* | TObject ( (["java";"lang"], "Integer"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx ([], "Int") []) ] *)
	(* | TObject ( (["java";"lang"], "Double"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx ([], "Float") []) ] *)
	(* | TObject ( (["java";"lang"], "Float"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx ([], "Single") []) ] *)
	(* | TObject ( (["java";"lang"], "Boolean"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx ([], "Bool") []) ] *)
	(* | TObject ( (["java";"lang"], "Byte"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx (["java";"types"], "Int8") []) ] *)
	(* | TObject ( (["java";"lang"], "Character"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx (["java";"types"], "Char16") []) ] *)
	(* | TObject ( (["java";"lang"], "Short"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx (["java";"types"], "Int16") []) ] *)
	(* | TObject ( (["java";"lang"], "Long"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx (["haxe"], "Int64") []) ] *)
	(** other std types *)
	| TObject ( (["java";"lang"], "Object"), [] ) -> mk_type_path ctx ([], "Dynamic") []
	| TObject ( (["java";"lang"], "String"), [] ) -> mk_type_path ctx ([], "String") []
	| TObject ( (["java";"lang"], "Enum"), [_] ) -> mk_type_path ctx ([], "EnumValue") []
	(** other types *)
	| TObject ( path, [] ) ->
		(match lookup_jclass ctx.jcom path with
		| Some (jcl, _, _) -> mk_type_path ctx path (List.map (fun _ -> convert_arg ctx p TAny) jcl.ctypes)
		| None -> mk_type_path ctx path [])
	| TObject ( path, args ) -> mk_type_path ctx path (List.map (convert_arg ctx p) args)
	| TObjectInner (pack, (name, params) :: inners) ->
			let actual_param = match List.rev inners with
			| (_, p) :: _ -> p
			| _ -> assert false in
			mk_type_path ctx (pack, name ^ "$" ^ String.concat "$" (List.map fst inners)) (List.map (fun param -> convert_arg ctx p param) actual_param)
	| TObjectInner (pack, inners) -> assert false
	| TArray (jsig, _) -> mk_type_path ctx (["java"], "NativeArray") [ TPType (convert_signature ctx p jsig,null_pos) ]
	| TMethod _ -> JReader.error "TMethod cannot be converted directly into Complex Type"
	| TTypeParameter s -> (match ctx.jtparams with
		| cur :: others ->
			if has_tparam s cur then
				mk_type_path ctx ([], s) []
			else begin
				if ctx.jcom.verbose && not(List.exists (has_tparam s) others) then print_endline ("Type parameter " ^ s ^ " was not found while building type!");
				mk_type_path ctx ([], "Dynamic") []
			end
		| _ ->
			if ctx.jcom.verbose then print_endline ("Empty type parameter stack!");
			mk_type_path ctx ([], "Dynamic") [])

let convert_constant ctx p const =
	Option.map_default (function
		| ConstString s -> Some (EConst (String (s,Double)), p)
		| ConstInt i -> Some (EConst (Int (Printf.sprintf "%ld" i)), p)
		| ConstFloat f | ConstDouble f -> Some (EConst (Float (Printf.sprintf "%E" f)), p)
		| _ -> None) None const

let rec same_sig parent jsig =
	match jsig with
	| TObject (p,targs) -> parent = p || List.exists (function | TType (_,s) -> same_sig parent s | _ -> false) targs
	| TObjectInner(p, ntargs) ->
			parent = (p, String.concat "$" (List.map fst ntargs)) ||
			List.exists (fun (_,targs) -> List.exists (function | TType(_,s) -> same_sig parent s | _ -> false) targs) ntargs
	| TArray(s,_) -> same_sig parent s
	| _ -> false

let convert_param ctx p parent param =
	let name, constraints = match param with
		| (name, Some extends_sig, implem_sig) ->
			name, extends_sig :: implem_sig
		| (name, None, implemem_sig) ->
			name, implemem_sig
		in
		let constraints = List.map (fun s -> if same_sig parent s then (TObject( (["java";"lang"], "Object"), [])) else s) constraints in
		{
			tp_name = jname_to_hx name,null_pos;
			tp_params = [];
			tp_constraints = List.map (fun t -> convert_signature ctx p t,null_pos) constraints;
			tp_meta = [];
		}

let get_type_path ctx ct = match ct with | CTPath p -> p | _ -> assert false

let is_override field =
	List.exists (function | AttrVisibleAnnotations [{ ann_type = TObject( (["java";"lang"], "Override"), _ ) }] -> true | _ -> false) field.jf_attributes

let mk_override field =
	{ field with jf_attributes = ((AttrVisibleAnnotations [{ ann_type = TObject( (["java";"lang"], "Override"), [] ); ann_elements = [] }]) :: field.jf_attributes) }

let del_override field =
	{ field with jf_attributes = List.filter (fun a -> not (is_override_attrib a)) field.jf_attributes }

let get_canonical ctx p pack name =
	(Meta.JavaCanonical, [EConst (String (String.concat "." pack,Double)), p; EConst (String (name,Double)), p], p)

let convert_java_enum ctx p pe =
	let meta = ref (get_canonical ctx p (fst pe.cpath) (snd pe.cpath) :: [Meta.Native, [EConst (String (real_java_path ctx pe.cpath,Double) ), p], p ]) in
	let data = ref [] in
	List.iter (fun f ->
		(* if List.mem JEnum f.jf_flags then *)
		match f.jf_vmsignature with
		| TObject( path, [] ) when path = pe.cpath && List.mem JStatic f.jf_flags && List.mem JFinal f.jf_flags ->
			data := { ec_name = f.jf_name,null_pos; ec_doc = None; ec_meta = []; ec_args = []; ec_pos = p; ec_params = []; ec_type = None; } :: !data;
		| _ -> ()
	) pe.cfields;

	EEnum {
		d_name = jname_to_hx (snd pe.cpath),null_pos;
		d_doc = None;
		d_params = []; (* enums never have type parameters *)
		d_meta = !meta;
		d_flags = [EExtern];
		d_data = List.rev !data;
	}

	let convert_java_field ctx p jc field =
		let p = { p with pfile =	p.pfile ^" (" ^field.jf_name ^")" } in
		let cff_doc = None in
		let cff_pos = p in
		let cff_meta = ref [] in
		let cff_access = ref [] in
		let cff_name = match field.jf_name with
			| "<init>" -> "new"
			| "<clinit>"-> raise Exit (* __init__ field *)
			| name when String.length name > 5 ->
					(match String.sub name 0 5 with
					| "__hx_" | "this$" -> raise Exit
					| _ -> name)
			| name -> name
		in
		let jf_constant = ref field.jf_constant in
		let readonly = ref false in

		List.iter (function
			| JPublic -> cff_access := APublic :: !cff_access
			| JPrivate -> raise Exit (* private instances aren't useful on externs *)
			| JProtected ->
				cff_meta := (Meta.Protected, [], p) :: !cff_meta;
				cff_access := APrivate :: !cff_access
			| JStatic -> cff_access := AStatic :: !cff_access
			| JFinal ->
				cff_meta := (Meta.Final, [], p) :: !cff_meta;
				(match field.jf_kind, field.jf_vmsignature, field.jf_constant with
				| JKField, TObject _, _ ->
					jf_constant := None
				| JKField, _, Some _ ->
					readonly := true;
					jf_constant := None;
				| _ -> jf_constant := None)
			(* | JSynchronized -> cff_meta := (Meta.Synchronized, [], p) :: !cff_meta *)
			| JVolatile -> cff_meta := (Meta.Volatile, [], p) :: !cff_meta
			| JTransient -> cff_meta := (Meta.Transient, [], p) :: !cff_meta
			(* | JVarArgs -> cff_meta := (Meta.VarArgs, [], p) :: !cff_meta *)
			| _ -> ()
		) field.jf_flags;

		List.iter (function
			| AttrDeprecated when jc.cpath <> (["java";"util"],"Date") -> cff_meta := (Meta.Deprecated, [], p) :: !cff_meta
			(* TODO: pass anotations as @:meta *)
			| AttrVisibleAnnotations ann ->
				List.iter (function
					| { ann_type = TObject( (["java";"lang"], "Override"), [] ) } ->
						cff_access := AOverride :: !cff_access
					| _ -> ()
				) ann
			| _ -> ()
		) field.jf_attributes;

		List.iter (fun jsig ->
			match convert_signature ctx p jsig with
				| CTPath path ->
					cff_meta := (Meta.Throws, [Ast.EConst (Ast.String (s_type_path (path.tpackage,path.tname),Double)), p],p) :: !cff_meta
				| _ -> ()
		) field.jf_throws;

		let kind = match field.jf_kind with
			| JKField when !readonly ->
				FProp (("default",null_pos), ("null",null_pos), Some (convert_signature ctx p field.jf_signature,null_pos), None)
			| JKField ->
				FVar (Some (convert_signature ctx p field.jf_signature,null_pos), None)
			| JKMethod ->
				match field.jf_signature with
				| TMethod (args, ret) ->
					let old_types = ctx.jtparams in
					(match ctx.jtparams with
					| c :: others -> ctx.jtparams <- (c @ field.jf_types) :: others
					| [] -> ctx.jtparams <- field.jf_types :: []);
					let i = ref 0 in
					let args = List.map (fun s ->
						incr i;
						("param" ^ string_of_int !i,null_pos), false, [], Some(convert_signature ctx p s,null_pos), None
					) args in
					let t = Option.map_default (convert_signature ctx p) (mk_type_path ctx ([], "Void") []) ret in
					cff_meta := (Meta.Overload, [], p) :: !cff_meta;

					let types = List.map (function
						| (name, Some ext, impl) ->
							{
								tp_name = name,null_pos;
								tp_params = [];
								tp_constraints = List.map (fun t -> convert_signature ctx p t,null_pos) (ext :: impl);
								tp_meta = [];
							}
						| (name, None, impl) ->
							{
								tp_name = name,null_pos;
								tp_params = [];
								tp_constraints = List.map (fun t -> convert_signature ctx p t,null_pos) (impl);
								tp_meta = [];
							}
					) field.jf_types in
					ctx.jtparams <- old_types;

					FFun ({
						f_params = types;
						f_args = args;
						f_type = Some (t,null_pos);
						f_expr = None
					})
				| _ -> error "Method signature was expected" p
		in
		let cff_name, cff_meta =
			match String.get cff_name 0 with
				| '%' ->
					let name = (String.sub cff_name 1 (String.length cff_name - 1)) in
					if not (is_haxe_keyword name) then
						cff_meta := (Meta.Deprecated, [EConst(String(
							"This static field `_" ^ name ^ "` is deprecated and will be removed in later versions. Please use `" ^ name ^ "` instead",Double)
						),p], p) :: !cff_meta;
					"_" ^ name,
					(Meta.Native, [EConst (String (name,Double) ), cff_pos], cff_pos) :: !cff_meta
				| _ ->
					match String.nsplit cff_name "$" with
						| [ no_dollar ] ->
							cff_name, !cff_meta
						| parts ->
							String.concat "_" parts,
							(Meta.Native, [EConst (String (cff_name,Double) ), cff_pos], cff_pos) :: !cff_meta
		in
		if PMap.mem "java_loader_debug" ctx.jcom.defines.Define.values then
			Printf.printf "\t%s%sfield %s : %s\n" (if List.mem AStatic !cff_access then "static " else "") (if List.mem AOverride !cff_access then "override " else "") cff_name (s_sig field.jf_signature);

		{
			cff_name = cff_name,null_pos;
			cff_doc = cff_doc;
			cff_pos = cff_pos;
			cff_meta = cff_meta;
			cff_access = !cff_access;
			cff_kind = kind
		}

	let rec japply_params params jsig = match params with
	| [] -> jsig
	| _ -> match jsig with
		| TTypeParameter s -> (try
			List.assoc s params
		with | Not_found -> jsig)
		| TObject(p,tl) ->
			TObject(p, args params tl)
		| TObjectInner(sl, stll) ->
			TObjectInner(sl, List.map (fun (s,tl) -> (s, args params tl)) stll)
		| TArray(s,io) ->
			TArray(japply_params params s, io)
		| TMethod(sl, sopt) ->
			TMethod(List.map (japply_params params) sl, Option.map (japply_params params) sopt)
		| _ -> jsig

	and args params tl = match params with
	| [] -> tl
	| _ -> List.map (function
		| TAny -> TAny
		| TType(w,s) -> TType(w,japply_params params s)) tl

	let mk_params jtypes = List.map (fun (s,_,_) -> (s,TTypeParameter s)) jtypes

	let convert_java_class ctx p jc =
		match List.mem JEnum jc.cflags with
		| true -> (* is enum *)
				[convert_java_enum ctx p jc]
		| false ->
			let flags = ref [HExtern] in
			if PMap.mem "java_loader_debug" ctx.jcom.defines.Define.values then begin
				let sup = jc.csuper :: jc.cinterfaces in
				print_endline ("converting " ^ (if List.mem JAbstract jc.cflags then "abstract " else "") ^ JData.path_s jc.cpath ^ " : " ^ (String.concat ", " (List.map s_sig sup)));
			end;
			(* todo: instead of JavaNative, use more specific definitions *)
			let meta = ref [Meta.JavaNative, [], p; Meta.Native, [EConst (String (real_java_path ctx jc.cpath,Double) ), p], p; get_canonical ctx p (fst jc.cpath) (snd jc.cpath)] in
			let force_check = Common.defined ctx.jcom Define.ForceLibCheck in
			if not force_check then
				meta := (Meta.LibType,[],p) :: !meta;

			let is_interface = ref false in
			List.iter (fun f -> match f with
				| JFinal -> meta := (Meta.Final, [], p) :: !meta
				| JInterface ->
						is_interface := true;
						flags := HInterface :: !flags
				| JAbstract -> meta := (Meta.Abstract, [], p) :: !meta
				| JAnnotation -> meta := (Meta.Annotation, [], p) :: !meta
				| _ -> ()
			) jc.cflags;

			(match jc.csuper with
				| TObject( (["java";"lang"], "Object"), _ ) -> ()
				| TObject( (["haxe";"lang"], "HxObject"), _ ) -> meta := (Meta.HxGen,[],p) :: !meta
				| _ -> flags := HExtends (get_type_path ctx (convert_signature ctx p jc.csuper),null_pos) :: !flags
			);

			List.iter (fun i ->
				match i with
				| TObject ( (["haxe";"lang"], "IHxObject"), _ ) -> meta := (Meta.HxGen,[],p) :: !meta
				| _ -> flags :=
					if !is_interface then
						HExtends (get_type_path ctx (convert_signature ctx p i),null_pos) :: !flags
					else
						HImplements (get_type_path ctx (convert_signature ctx p i),null_pos) :: !flags
			) jc.cinterfaces;

			let fields = ref [] in
			let jfields = ref [] in

			if jc.cpath <> (["java";"lang"], "CharSequence") then
				List.iter (fun f ->
					try
						if !is_interface && List.mem JStatic f.jf_flags then
							()
						else begin
							fields := convert_java_field ctx p jc f :: !fields;
							jfields := f :: !jfields
						end
					with
						| Exit -> ()
				) (jc.cfields @ jc.cmethods);

			(* make sure the throws types are imported correctly *)
			let imports = List.concat (List.map (fun f ->
				List.map (fun jsig ->
					match convert_signature ctx p jsig with
						| CTPath path ->
							let pos = { p with pfile = p.pfile ^ " (" ^ f.jf_name ^" @:throws)" } in
							EImport( List.map (fun s -> s,pos) (path.tpackage @ [path.tname]), INormal )
						| _ -> assert false
				) f.jf_throws
			) jc.cmethods) in

			(EClass {
				d_name = jname_to_hx (snd jc.cpath),null_pos;
				d_doc = None;
				d_params = List.map (convert_param ctx p jc.cpath) jc.ctypes;
				d_meta = !meta;
				d_flags = !flags;
				d_data = !fields;
			}) :: imports

	let create_ctx com =
		{
			jcom = com;
			jtparams = [];
		}

	let rec has_type_param = function
		| TTypeParameter _ -> true
		| TMethod (lst, opt) -> List.exists has_type_param lst || Option.map_default has_type_param false opt
		| TArray (s,_) -> has_type_param s
		| TObjectInner (_, stpl) -> List.exists (fun (_,sigs) -> List.exists has_type_param_arg sigs) stpl
		| TObject(_, pl) -> List.exists has_type_param_arg pl
		| _ -> false

	and has_type_param_arg = function | TType(_,s) -> has_type_param s | _ -> false

let rec japply_params jparams jsig = match jparams with
	| [] -> jsig
	| _ ->
		match jsig with
		| TObject(path,p) ->
			TObject(path, List.map (japply_params_tp jparams ) p)
		| TObjectInner(sl,stargl) ->
			TObjectInner(sl,List.map (fun (s,targ) -> (s, List.map (japply_params_tp jparams) targ)) stargl)
		| TArray(jsig,io) ->
			TArray(japply_params jparams jsig,io)
		| TMethod(args,ret) ->
			TMethod(List.map (japply_params jparams ) args, Option.map (japply_params jparams ) ret)
		| TTypeParameter s -> (try
			List.assoc s jparams
		with | Not_found -> jsig)
		| _ -> jsig


and japply_params_tp jparams jtype_argument = match jtype_argument with
	| TAny -> TAny
	| TType(w,jsig) -> TType(w,japply_params jparams jsig)

let mk_jparams jtypes params = match jtypes, params with
	| [], [] -> []
	| _, [] -> List.map (fun (s,_,_) -> s, TObject( (["java";"lang"], "Object"), [] ) ) jtypes
	| _ -> List.map2 (fun (s,_,_) jt -> match jt with
		| TAny -> s, TObject((["java";"lang"],"Object"),[])
		| TType(_,jsig) -> s, jsig) jtypes params

let rec compatible_signature_arg ?arg_test f1 f2 =
	let arg_test = match arg_test with
		| None -> (fun _ _ -> true)
		| Some a -> a
	in
	if f1 = f2 then
		true
	else match f1, f2 with
	| TObject(p,a), TObject(p2,a2) -> p = p2 && arg_test a a2
	| TObjectInner(sl, stal), TObjectInner(sl2, stal2) -> sl = sl2 && List.map fst stal = List.map fst stal2
	| TArray(s,_) , TArray(s2,_) -> compatible_signature_arg s s2
	| TTypeParameter t1 , TTypeParameter t2 -> t1 = t2
	| _ -> false

let rec compatible_param p1 p2 = match p1, p2 with
	| TType (_,s1), TType(_,s2) -> compatible_signature_arg ~arg_test:compatible_tparams s1 s2
	| TAny, TType(_, TObject( (["java";"lang"],"Object"), _ )) -> true
	| TType(_, TObject( (["java";"lang"],"Object"), _ )), TAny -> true
	| _ -> false

and compatible_tparams p1 p2 = try match p1, p2 with
	| [], [] -> true
	| _, [] ->
		let p2 = List.map (fun _ -> TAny) p1 in
		List.for_all2 compatible_param p1 p2
	| [], _ ->
		let p1 = List.map (fun _ -> TAny) p2 in
		List.for_all2 compatible_param p1 p2
	| _, _ ->
		List.for_all2 compatible_param p1 p2
	with | Invalid_argument _ -> false

let get_adapted_sig f f2 = match f.jf_types with
	| [] ->
		f.jf_signature
	| _ ->
		let jparams = mk_jparams f.jf_types (List.map (fun (s,_,_) -> TType(WNone, TTypeParameter s)) f2.jf_types) in
		japply_params jparams f.jf_signature

let compatible_methods f1 f2 =
	if List.length f1.jf_types <> List.length f2.jf_types then
		false
	else match (get_adapted_sig f1 f2), f2.jf_signature with
	| TMethod(a1,_), TMethod(a2,_) when List.length a1 = List.length a2 ->
		List.for_all2 compatible_signature_arg a1 a2
	| _ -> false

let jcl_from_jsig com jsig =
	let path, params = match jsig with
	| TObject(path, params) ->
		path,params
	| TObjectInner(sl, stll) ->
		let last_params = ref [] in
		let real_path = sl, String.concat "$" (List.map (fun (s,p) -> last_params := p; s) stll) in
		real_path, !last_params
	| _ -> raise Not_found
	in
	match lookup_jclass com path with
	| None -> raise Not_found
	| Some(c,_,_) -> c,params

let jclass_with_params com cls params = try
	match cls.ctypes with
	| [] -> cls
	| _ ->
		let jparams = mk_jparams cls.ctypes params in
		{ cls with
			cfields = List.map (fun f -> { f with jf_signature = japply_params jparams f.jf_signature }) cls.cfields;
			cmethods = List.map (fun f -> { f with jf_signature = japply_params jparams f.jf_signature }) cls.cmethods;
			csuper = japply_params jparams cls.csuper;
			cinterfaces = List.map (japply_params jparams) cls.cinterfaces;
		}
	with Invalid_argument _ ->
		if com.verbose then prerr_endline ("Differing parameters for class: " ^ s_type_path cls.cpath);
		cls

let is_object = function | TObject( (["java";"lang"], "Object"), [] ) -> true | _ -> false

let is_tobject = function | TObject _ | TObjectInner _ -> true | _ -> false

let simplify_args args =
	if List.for_all (function | TAny -> true | _ -> false) args then [] else args

let compare_type com s1 s2 =
	if s1 = s2 then
		0
	else if not (is_tobject s1) then
		if is_tobject s2 then (* Dynamic *)
			1
		else if compatible_signature_arg s1 s2 then
			0
		else
			raise Exit
	else if not (is_tobject s2) then
		-1
	else begin
		let rec loop ?(first_error=true) s1 s2 : bool =
			if is_object s1 then
				s1 = s2
			else if compatible_signature_arg s1 s2 then begin
				let p1, p2 = match s1, s2 with
				| TObject(_, p1), TObject(_,p2) ->
					p1, p2
				| TObjectInner(_, npl1), TObjectInner(_, npl2) ->
					snd (List.hd (List.rev npl1)), snd (List.hd (List.rev npl2))
				| _ -> assert false (* not tobject *)
				in
				let p1, p2 = simplify_args p1, simplify_args p2 in
				let lp1 = List.length p1 in
				let lp2 = List.length p2 in
				if lp1 > lp2 then
					true
				else if lp2 > lp1 then
					false
				else begin
					(* if compatible tparams, it's fine *)
					if not (compatible_tparams p1 p2) then
						raise Exit; (* meaning: found, but incompatible type parameters *)
					true
				end
			end else try
				let c, p = jcl_from_jsig com s1 in
				let jparams = mk_jparams c.ctypes p in
				let super = japply_params jparams c.csuper in
				let implements = List.map (japply_params jparams) c.cinterfaces in
				loop ~first_error:first_error super s2 || List.exists (fun super -> loop ~first_error:first_error super s2) implements
			with | Not_found ->
				prerr_endline ("-java-lib: The type " ^ (s_sig s1) ^ " is referred but was not found. Compilation may not occur correctly.");
				prerr_endline "Did you forget to include a needed lib?";
				if first_error then
					not (loop ~first_error:false s2 s1)
				else
					false
		in
		if loop s1 s2 then
			if loop s2 s1 then
				0
			else
				1
		else
			if loop s2 s1 then
				-1
			else
				-2
	end

(* given a list of same overload functions, choose the best (or none) *)
let select_best com flist =
	let rec loop cur_best = function
		| [] ->
			Some cur_best
		| f :: flist -> match get_adapted_sig f cur_best, cur_best.jf_signature with
			| TMethod(_,Some r), TMethod(_, Some r2) -> (try
				match compare_type com r r2 with
				| 0 -> (* same type - select any of them *)
					loop cur_best flist
				| 1 ->
					loop f flist
				| -1 ->
					loop cur_best flist
				| -2 -> (* error - no type is compatible *)
					if com.verbose then prerr_endline (f.jf_name ^ ": The types " ^ (s_sig r) ^ " and " ^ (s_sig r2) ^ " are incompatible");
					(* bet that the current best has "beaten" other types *)
					loop cur_best flist
				| _ -> assert false
			with | Exit -> (* incompatible type parameters *)
				(* error mode *)
				if com.verbose then prerr_endline (f.jf_name ^ ": Incompatible argument return signatures: " ^ (s_sig r) ^ " and " ^ (s_sig r2));
				None)
			| TMethod _, _ -> (* select the method *)
				loop f flist
			| _ ->
				loop cur_best flist
	in
	match loop (List.hd flist) (List.tl flist) with
	| Some f ->
		Some f
	| None -> match List.filter (fun f -> not (is_override f)) flist with
		(* error mode; take off all override methods *)
		| [] -> None
		| f :: [] -> Some f
		| f :: flist -> Some f (* pick one *)

(**** begin normalize_jclass helpers ****)

let fix_overrides_jclass com cls =
	let force_check = Common.defined com Define.ForceLibCheck in
	let methods = if force_check then List.map (fun f -> del_override f) cls.cmethods else cls.cmethods in
	let cmethods = methods in
	let super_fields = [] in
	let super_methods = [] in
	let nonstatics = List.filter (fun f -> not (List.mem JStatic f.jf_flags)) (cls.cfields @ cls.cmethods) in

	let is_pub = fun f -> List.exists (function | JPublic | JProtected -> true | _ -> false) f.jf_flags in
	let cmethods, super_fields = if not (List.mem JInterface cls.cflags) then
		List.filter is_pub cmethods,
		List.filter is_pub super_fields
	else
		cmethods,super_fields
	in

	let rec loop cls super_methods super_fields cmethods nonstatics = try
		match cls.csuper with
		| TObject((["java";"lang"],"Object"),_) ->
				super_methods,super_fields,cmethods,nonstatics
		| _ ->
			let cls, params = jcl_from_jsig com cls.csuper in
			let cls = jclass_with_params com cls params in
			let nonstatics = (List.filter (fun f -> (List.mem JStatic f.jf_flags)) (cls.cfields @ cls.cmethods)) @ nonstatics in
			let super_methods = cls.cmethods @ super_methods in
			let super_fields = cls.cfields @ super_fields in
			let cmethods = if force_check then begin
				let overriden = ref [] in
				let cmethods = List.map (fun jm ->
					(* TODO rewrite/standardize empty spaces *)
					if not (is_override jm) && not (List.mem JStatic jm.jf_flags) && List.exists (fun msup ->
						let ret = msup.jf_name = jm.jf_name && not(List.mem JStatic msup.jf_flags) && compatible_methods msup jm in
						if ret then begin
							let f = mk_override msup in
							overriden := { f with jf_flags = jm.jf_flags } :: !overriden
						end;
						ret
					) cls.cmethods then
						mk_override jm
					else
						jm
				) cmethods in
				!overriden @ cmethods
			end else
				cmethods
			in
			loop cls super_methods super_fields cmethods nonstatics
		with | Not_found ->
			super_methods,super_fields,cmethods,nonstatics
	in
	loop cls super_methods super_fields cmethods nonstatics

let normalize_jclass com cls =
	(* after adding the noCheck metadata, this option will annotate what changes were needed *)
	(* and that are now deprecated *)
	let force_check = Common.defined com Define.ForceLibCheck in
	(* fix overrides *)
	let super_methods, super_fields, cmethods, nonstatics = fix_overrides_jclass com cls in
	let all_methods = cmethods @ super_methods in

	(* look for interfaces and add missing implementations (may happen on abstracts or by vmsig differences *)
	(* (libType): even with libType enabled, we need to add these missing fields - otherwise we won't be able to use them from Haxe *)
	let added_interface_fields = ref [] in
	let rec loop_interface abstract cls iface = try
		match iface with
			| TObject ((["java";"lang"],"Object"), _) -> ()
			| TObject (path,_) when path = cls.cpath -> ()
			| _ ->
				let cif, params = jcl_from_jsig com iface in
				let cif = jclass_with_params com cif params in
				List.iter (fun jf ->
					if not(List.mem JStatic jf.jf_flags) && not (List.exists (fun jf2 -> jf.jf_name = jf2.jf_name && not (List.mem JStatic jf2.jf_flags) && jf.jf_signature = jf2.jf_signature) all_methods) then begin
						let jf = if abstract && force_check then del_override jf else jf in
						let jf = { jf with jf_flags = JPublic :: jf.jf_flags } in (* interfaces implementations are always public *)

						added_interface_fields := jf :: !added_interface_fields;
					end
				) cif.cmethods;
				(* we don't need to loop again in the interface unless we are in an abstract class, since these interfaces are already normalized *)
				if abstract then List.iter (loop_interface abstract cif) cif.cinterfaces;
		with Not_found -> ()
	in
	List.iter (loop_interface (List.mem JAbstract cls.cflags) cls) cls.cinterfaces;
	let nonstatics = !added_interface_fields @ nonstatics in
	let cmethods = !added_interface_fields @ cmethods in

	(* for each added field in the interface, lookup in super_methods possible methods to include *)
	(* so we can choose the better method still *)
	let cmethods = if not force_check then
		cmethods
	else
		List.fold_left (fun cmethods im ->
			(* see if any of the added_interface_fields need to be declared as override *)
			let f = List.find_all (fun jf -> jf.jf_name = im.jf_name && compatible_methods jf im) super_methods in
			let f = List.map mk_override f in
			f @ cmethods
		) cmethods !added_interface_fields;
	in

	(* take off equals, hashCode and toString from interface *)
	let cmethods = if List.mem JInterface cls.cflags then List.filter (fun jf -> match jf.jf_name, jf.jf_vmsignature with
			| "equals", TMethod([TObject( (["java";"lang"],"Object"), _)],_)
			| "hashCode", TMethod([], _)
			| "toString", TMethod([], _) -> false
			| _ -> true
	) cmethods
	else
		cmethods
	in

	(* change field name to not collide with haxe keywords and with static/non-static members *)
	let fold_field acc f =
		let change, both = match f.jf_name with
		| _ when List.mem JStatic f.jf_flags && List.exists (fun f2 -> f.jf_name = f2.jf_name) nonstatics -> true, true
		| _ -> is_haxe_keyword f.jf_name, false
		in
		let f2 = if change then
				{ f with jf_name = "%" ^ f.jf_name }
			else
				f
		in
		if both then f :: f2 :: acc else f2 :: acc
	in

	(* change static fields that have the same name as methods *)
	let cfields = List.fold_left fold_field [] cls.cfields in
	let cmethods = List.fold_left fold_field [] cmethods in
	(* take off variable fields that have the same name as methods *)
	(* and take off variables that already have been declared *)
	let filter_field f f2 = f != f2 && (List.mem JStatic f.jf_flags = List.mem JStatic f2.jf_flags) && f.jf_name = f2.jf_name && f2.jf_kind <> f.jf_kind in
	let cfields = List.filter (fun f ->
		if List.mem JStatic f.jf_flags then
			not (List.exists (filter_field f) cmethods)
		else
			not (List.exists (filter_field f) nonstatics) && not (List.exists (fun f2 -> f != f2 && f.jf_name = f2.jf_name && not (List.mem JStatic f2.jf_flags)) super_fields) ) cfields
	in
	(* now filter any method that clashes with a field - on a superclass *)
	let cmethods = if force_check then List.filter (fun f ->
		if List.mem JStatic f.jf_flags then
			true
		else
			not (List.exists (filter_field f) super_fields) ) cmethods
	else
		cmethods
	in
	(* removing duplicate fields. They are there because of return type covariance in Java *)
	(* Also, if a method overrides a previous definition, and changes a type parameters' variance, *)
	(* we will take it off *)
	(* this means that some rare codes will never compile on Haxe, but unless Haxe adds variance support *)
	(* I can't see how this can be any different *)
	let rec loop acc = function
		| [] -> acc
		| f :: cmeths ->
			match List.partition (fun f2 -> f.jf_name = f2.jf_name && compatible_methods f f2) cmeths with
			| [], cmeths ->
				loop (f :: acc) cmeths
			| flist, cmeths -> match select_best com (f :: flist) with
				| None ->
					loop acc cmeths
				| Some f ->
					loop (f :: acc) cmeths
	in
	(* last pass: take off all cfields that are internal / private (they won't be accessible anyway) *)
	let cfields = List.filter(fun f -> List.exists (fun f -> f = JPublic || f = JProtected) f.jf_flags) cfields in
	let cmethods = loop [] cmethods in
	{ cls with cfields = cfields; cmethods = cmethods }

(**** end normalize_jclass helpers ****)

let get_classes_zip zip =
	let ret = ref [] in
	List.iter (function
		| { Zip.is_directory = false; Zip.filename = f } when (String.sub (String.uncapitalize f) (String.length f - 6) 6) = ".class" && not (String.exists f "$") ->
				(match List.rev (String.nsplit f "/") with
				| clsname :: pack ->
					if not (String.contains clsname '$') then begin
						let path = jpath_to_hx (List.rev pack, String.sub clsname 0 (String.length clsname - 6)) in
						ret := path :: !ret
					end
				| _ ->
						ret := ([], jname_to_hx f) :: !ret)
		| _ -> ()
	) (Zip.entries zip);
	!ret

let add_java_lib com file std =
	let file = if Sys.file_exists file then
		file
	else try Common.find_file com file with
		| Not_found -> try Common.find_file com (file ^ ".jar") with
		| Not_found ->
			failwith ("Java lib " ^ file ^ " not found")
	in
	let hxpack_to_jpack = Hashtbl.create 16 in
	let get_raw_class, close, list_all_files =
		(* check if it is a directory or jar file *)
		match (Unix.stat file).st_kind with
		| S_DIR -> (* open classes directly from directory *)
			let all = ref [] in
			let rec iter_files pack dir path = try
				let file = Unix.readdir dir in
				let filepath = path ^ "/" ^ file in
				(if String.ends_with file ".class" then
					let name = String.sub file 0 (String.length file - 6) in
					let path = jpath_to_hx (pack,name) in
					if not (String.exists file "$") then all := path :: !all;
					Hashtbl.add hxpack_to_jpack path (pack,name)
				else if (Unix.stat filepath).st_kind = S_DIR && file <> "." && file <> ".." then
					let pack = pack @ [file] in
					iter_files (pack) (Unix.opendir filepath) filepath);
				iter_files pack dir path
			with | End_of_file | Unix.Unix_error _ ->
				Unix.closedir dir
			in
			iter_files [] (Unix.opendir file) file;
			let all = !all in

			(fun (pack, name) ->
				let real_path = file ^ "/" ^ (String.concat "/" pack) ^ "/" ^ (name ^ ".class") in
				try
					let data = Std.input_file ~bin:true real_path in
					Some(JReader.parse_class (IO.input_string data), real_path, real_path)
				with
					| _ -> None), (fun () -> ()), (fun () -> all)
		| _ -> (* open zip file *)
			let closed = ref false in
			let zip = ref (Zip.open_in file) in
			let check_open () =
				if !closed then begin
					prerr_endline ("JAR file " ^ file ^ " already closed"); (* if this happens, find when *)
					zip := Zip.open_in file;
					closed := false
				end
			in
			List.iter (function
				| { Zip.is_directory = false; Zip.filename = filename } when String.ends_with filename ".class" ->
					let pack = String.nsplit filename "/" in
					(match List.rev pack with
						| [] -> ()
						| name :: pack ->
							let name = String.sub name 0 (String.length name - 6) in
							let pack = List.rev pack in
							Hashtbl.add hxpack_to_jpack (jpath_to_hx (pack,name)) (pack,name))
				| _ -> ()
			) (Zip.entries !zip);
			(fun (pack, name) ->
				check_open();
				try
					let location = (String.concat "/" (pack @ [name]) ^ ".class") in
					let entry = Zip.find_entry !zip location in
					let data = Zip.read_entry !zip entry in
					Some(JReader.parse_class (IO.input_string data), file, file ^ "@" ^ location)
				with
					| Not_found ->
						None),
			(fun () -> if not !closed then begin closed := true; Zip.close_in !zip end),
			(fun () -> check_open(); get_classes_zip !zip)
	in
	let cached_types = Hashtbl.create 12 in
	let get_raw_class path =
		try
			Hashtbl.find cached_types path
		with | Not_found -> try
			let pack, name = Hashtbl.find hxpack_to_jpack path in
			let try_file (pack,name) =
				match get_raw_class (pack,name) with
				| None ->
						Hashtbl.add cached_types path None;
						None
				| Some (i, p1, p2) ->
						Hashtbl.add cached_types path (Some(i,p1,p2)); (* type loop normalization *)
						let ret = Some (normalize_jclass com i, p1, p2) in
						Hashtbl.replace cached_types path ret;
						ret
			in
			try_file (pack,name)
		with Not_found ->
			None
	in
	let replace_canonical_name p pack name_original name_replace decl =
		let mk_meta name = (Meta.JavaCanonical, [EConst (String (String.concat "." pack,Double)), p; EConst(String (name,Double)), p], p) in
		let add_meta name metas =
			if Meta.has Meta.JavaCanonical metas then
				List.map (function
					| (Meta.JavaCanonical,[EConst (String (cpack,_)), _; EConst(String (cname,_)), _],_) ->
						let did_replace,name = String.replace cname name_original name_replace in
						if not did_replace then print_endline (cname ^ " -> " ^ name_original ^ " -> " ^ name_replace);
						mk_meta name
					| m -> m
				) metas
			else
				mk_meta name :: metas
		in
		match decl with
			| EClass c ->
				EClass { c with d_meta = add_meta (fst c.d_name) c.d_meta }
			| EEnum e ->
				EEnum { e with d_meta = add_meta (fst e.d_name) e.d_meta }
			| EAbstract a ->
				EAbstract { a with d_meta = add_meta (fst a.d_name) a.d_meta }
			| d -> d
	in
	let rec build ctx path p types =
		try
			if List.mem path !types then
				None
			else begin
				let first = match !types with
					| [ ["java";"lang"], "String" ] | [] -> true
					| p :: _ ->
						false
				in
				types := path :: !types;
				match get_raw_class path, path with
				| None, ([], c) -> build ctx (["haxe";"root"], c) p types
				| None, _ -> None
				| Some (cls, real_path, pos_path), _ ->
						let is_disallowed_inner = first && String.exists (snd cls.cpath) "$" in
						let is_disallowed_inner = if is_disallowed_inner then begin
								let outer, inner = String.split (snd cls.cpath) "$" in
								match get_raw_class (fst path, outer) with
									| None -> false
									| _ -> true
							end else
								false
						in
						if is_disallowed_inner then
							None
						else begin
							if com.verbose then print_endline ("Parsed Java class " ^ (s_type_path cls.cpath));
							let old_types = ctx.jtparams in
							ctx.jtparams <- cls.ctypes :: ctx.jtparams;

							let pos = { pfile = pos_path; pmin = 0; pmax = 0; } in

							let pack = match fst path with | ["haxe";"root"] -> [] | p -> p in

							let ppath = Hashtbl.find hxpack_to_jpack path in
							let inner = List.fold_left (fun acc (path,out,_,_) ->
								let path = jpath_to_hx path in
								(if out <> Some ppath then
									acc
								else match build ctx path p types with
									| Some(_,(_, classes)) ->
										let base = snd ppath ^ "$" in
										(List.map (fun (def,p) ->
											replace_canonical_name p (fst ppath) base (snd ppath ^ ".") def, p) classes) @ acc
									| _ -> acc);
							) [] cls.cinner_types in

							(* add _Statics class *)
							let inner = try
								if not (List.mem JInterface cls.cflags) then raise Not_found;
								let smethods = List.filter (fun f -> List.mem JStatic f.jf_flags) cls.cmethods in
								let sfields = List.filter (fun f -> List.mem JStatic f.jf_flags) cls.cfields in
								if not (smethods <> [] || sfields <> []) then raise Not_found;
								let obj = TObject( (["java";"lang"],"Object"), []) in
								let ncls = convert_java_class ctx pos { cls with cmethods = smethods; cfields = sfields; cflags = []; csuper = obj; cinterfaces = []; cinner_types = []; ctypes = [] } in
								match ncls with
								| EClass c :: imports ->
									(EClass { c with d_name = (fst c.d_name ^ "_Statics"),snd c.d_name }, pos) :: inner @ List.map (fun i -> i,pos) imports
								| _ -> assert false
							with | Not_found ->
								inner
							in
							let inner_alias = ref SS.empty in
							List.iter (fun x ->
								match fst x with
								| EClass c ->
									inner_alias := SS.add (fst c.d_name) !inner_alias;
								| _ -> ()
							) inner;
							let alias_list = ref [] in
							List.iter (fun x ->
								match x with
								| (EClass c, pos) -> begin
									let parts = String.nsplit (fst c.d_name) "_24" in
									match parts with
										| _ :: _ ->
											let alias_name = String.concat "_" parts in
											if (not (SS.mem alias_name !inner_alias)) && (not (String.exists (snd path) "_24")) then begin
												let alias_def = ETypedef {
													d_name = alias_name,null_pos;
													d_doc = None;
													d_params = c.d_params;
													d_meta = [];
													d_flags = [];
													d_data = CTPath {
														tpackage = pack;
														tname = snd path;
														tparams = List.map (fun tp ->
															TPType (CTPath {
																tpackage = [];
																tname = fst tp.tp_name;
																tparams = [];
																tsub = None;
															},null_pos)
														) c.d_params;
														tsub = Some(fst c.d_name);
													},null_pos;
												} in
												inner_alias := SS.add alias_name !inner_alias;
												alias_list := (alias_def, pos) :: !alias_list;
											end
										| _ -> ()
								end
								| _ -> ()
							) inner;
							let inner = List.concat [!alias_list ; inner] in
							let classes = List.map (fun t -> t,pos) (convert_java_class ctx pos cls) in
							let imports, defs = List.partition (function | (EImport(_),_) -> true | _ -> false) (classes @ inner) in
							let ret = Some ( real_path, (pack, imports @ defs) ) in
							ctx.jtparams <- old_types;
							ret
						end
			end
		with
		| JReader.Error_message msg ->
			prerr_endline ("Class reader failed: " ^ msg);
			None
		| e ->
			if com.verbose then begin
				(* prerr_endline (Printexc.get_backtrace ()); requires ocaml 3.11 *)
				prerr_endline (Printexc.to_string e)
			end;
			None
	in
	let build path p = build (create_ctx com) path p (ref [["java";"lang"], "String"]) in
	let cached_files = ref None in
	let list_all_files () = match !cached_files with
		| None ->
				let ret = list_all_files () in
				cached_files := Some ret;
				ret
		| Some r -> r
	in

	(* TODO: add_dependency m mdep *)
	com.load_extern_type <- com.load_extern_type @ [build];
	com.java_libs <- (file, std, close, list_all_files, get_raw_class) :: com.java_libs

let before_generate con =
	let java_ver = try
			int_of_string (PMap.find "java_ver" con.defines.Define.values)
		with | Not_found ->
			Common.define_value con Define.JavaVer "7";
			7
	in
	if java_ver < 5 then failwith ("Java version is defined to target Java " ^ string_of_int java_ver ^ ", but the compiler can only output code to versions equal or superior to Java 5");
	let rec loop i =
		Common.raw_define con ("java" ^ (string_of_int i));
		if i > 0 then loop (i - 1)
	in
	loop java_ver
