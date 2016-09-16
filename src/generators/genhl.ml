(*
 * Copyright (C)2005-2015 Haxe Foundation
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
open Unix
open Ast
open Type
open Common
open Hlcode

(* compiler *)

type ('a,'b) lookup = {
	arr : 'b DynArray.t;
	mutable map : ('a, int) PMap.t;
}

(* not mutable, might be be shared *)
type method_capture = {
	c_map : (int, int) PMap.t;
	c_vars : tvar array;
	c_type : ttype;
}

type method_context = {
	mid : int;
	mregs : (int, ttype) lookup;
	mops : opcode DynArray.t;
	mret : ttype;
	mdebug : (int * int) DynArray.t;
	mutable mcaptured : method_capture;
	mutable mcontinues : (int -> unit) list;
	mutable mbreaks : (int -> unit) list;
	mutable mtrys : int;
	mutable mcaptreg : int;
	mutable mcurpos : (int * int);
}

type array_impl = {
	aall : tclass;
	abase : tclass;
	adyn : tclass;
	aobj : tclass;
	aui16 : tclass;
	ai32 : tclass;
	af32 : tclass;
	af64 : tclass;
}

type context = {
	com : Common.context;
	cglobals : (string, ttype) lookup;
	cstrings : (string, string) lookup;
	cfloats : (float, float) lookup;
	cints : (int32, int32) lookup;
	cnatives : (string, (string index * string index * ttype * functable index)) lookup;
	cfids : (string * path, unit) lookup;
	cfunctions : fundecl DynArray.t;
	optimize : bool;
	overrides : (string * path, bool) Hashtbl.t;
	defined_funs : (int,unit) Hashtbl.t;
	mutable dump_out : (unit IO.output) option;
	mutable cached_types : (path, ttype) PMap.t;
	mutable m : method_context;
	mutable anons_cache : (tanon * ttype) list;
	mutable method_wrappers : ((ttype * ttype), int) PMap.t;
	mutable rec_cache : (Type.t * ttype option ref) list;
	array_impl : array_impl;
	base_class : tclass;
	base_type : tclass;
	base_enum : tclass;
	core_type : tclass;
	core_enum : tclass;
	cdebug_files : (string, string) lookup;
}

(* --- *)

type access =
	| ANone
	| AGlobal of global
	| ALocal of reg
	| AStaticVar of global * ttype * field index
	| AStaticFun of fundecl index
	| AInstanceFun of texpr * fundecl index
	| AInstanceProto of texpr * field index
	| AInstanceField of texpr * field index
	| AArray of reg * (ttype * ttype) * reg
	| AVirtualMethod of texpr * field index
	| ADynamic of texpr * string index
	| AEnum of tenum * field index
	| ACaptured of field index

let is_to_string t =
	match follow t with
	| TFun([],r) -> (match follow r with TInst({ cl_path=[],"String" },[]) -> true | _ -> false)
	| _ -> false

let is_extern_field f =
	Type.is_extern_field f || (match f.cf_kind with Method MethNormal -> List.exists (fun (m,_,_) -> m = Meta.Custom ":hlNative") f.cf_meta | _ -> false)

let is_array_class name =
	match name with
	| "hl.types.ArrayDyn" | "hl.types.ArrayBasic_Int" | "hl.types.ArrayBasic_Float" | "hl.types.ArrayObj" -> true
	| _ -> false

let is_array_type t =
	match t with
	| HObj p -> is_array_class p.pname
	| _ -> false

let to_utf8 str p =
	let u8 = try
		UTF8.validate str;
		str;
	with
		UTF8.Malformed_code ->
			(* ISO to utf8 *)
			let b = UTF8.Buf.create 0 in
			String.iter (fun c -> UTF8.Buf.add_char b (UChar.of_char c)) str;
			UTF8.Buf.contents b
	in
	let ccount = ref 0 in
	UTF8.iter (fun c ->
		let c = UChar.code c in
		if (c >= 0xD800 && c <= 0xDFFF) || c >= 0x110000 then abort "Invalid unicode char" p;
		incr ccount;
		if c > 0x10000 then incr ccount;
	) u8;
	u8, !ccount

let type_size_bits = function
	| HUI8 | HBool -> 0
	| HUI16 -> 1
	| HI32 | HF32 -> 2
	| HF64 -> 3
	| _ -> assert false

let new_lookup() =
	{
		arr = DynArray.create();
		map = PMap.empty;
	}

let null_capture =
	{
		c_vars = [||];
		c_map = PMap.empty;
		c_type = HVoid;
	}

let lookup l v fb =
	try
		PMap.find v l.map
	with Not_found ->
		let id = DynArray.length l.arr in
		DynArray.add l.arr (Obj.magic 0);
		l.map <- PMap.add v id l.map;
		DynArray.set l.arr id (fb());
		id

let lookup_alloc l v =
	let id = DynArray.length l.arr in
	DynArray.add l.arr v;
	id

let method_context id t captured =
	{
		mid = id;
		mregs = new_lookup();
		mops = DynArray.create();
		mret = t;
		mbreaks = [];
		mcontinues = [];
		mcaptured = captured;
		mtrys = 0;
		mcaptreg = 0;
		mdebug = DynArray.create();
		mcurpos = (0,0);
	}

let field_name c f =
	s_type_path c.cl_path ^ ":" ^ f.cf_name

let efield_name e f =
	s_type_path e.e_path ^ ":" ^ f.ef_name

let underscore_class_name c = match c.cl_path with [],s -> s | p,s -> String.concat "_" p ^ "_" ^ s

let global_type ctx g =
	DynArray.get ctx.cglobals.arr g

let is_overriden ctx c f =
	Hashtbl.mem ctx.overrides (f.cf_name,c.cl_path)

let alloc_float ctx f =
	lookup ctx.cfloats f (fun() -> f)

let alloc_i32 ctx i =
	lookup ctx.cints i (fun() -> i)

let alloc_string ctx s =
	lookup ctx.cstrings s (fun() -> s)

let array_class ctx t =
	match t with
	| HI32 ->
		ctx.array_impl.ai32
	| HUI16 ->
		ctx.array_impl.aui16
	| HF32 ->
		ctx.array_impl.af32
	| HF64 ->
		ctx.array_impl.af64
	| HDyn ->
		ctx.array_impl.adyn
	| _ ->
		ctx.array_impl.aobj

let member_fun c t =
	match follow t with
	| TFun (args, ret) -> TFun (("this",false,TInst(c,[])) :: args, ret)
	| _ -> assert false

let rec unsigned t =
	match follow t with
	| TAbstract ({ a_path = [],"UInt" },_) -> true
	| TAbstract (a,pl) -> unsigned (Abstract.get_underlying_type a pl)
	| _ -> false

let set_curpos ctx p =
	let get_relative_path() =
		match Common.defined ctx.com Common.Define.AbsolutePath with
		| true -> if (Filename.is_relative p.pfile)
			then Filename.concat (Sys.getcwd()) p.pfile
			else p.pfile
		| false -> try
			(* lookup relative path *)
			let len = String.length p.pfile in
			let base = List.find (fun path ->
				let l = String.length path in
				len > l && String.sub p.pfile 0 l = path
			) ctx.com.Common.class_path in
			let l = String.length base in
			String.sub p.pfile l (len - l)
		with Not_found ->
			p.pfile
	in
	ctx.m.mcurpos <- (lookup ctx.cdebug_files p.pfile get_relative_path,Lexer.get_error_line p)

let rec to_type ?tref ctx t =
	match t with
	| TMono r ->
		(match !r with
		| None -> HDyn
		| Some t -> to_type ?tref ctx t)
	| TType (td,tl) ->
		let t = (try
			match !(List.assq t ctx.rec_cache) with
			| None -> abort "Unsupported recursive type" td.t_pos
			| Some t -> t
		with Not_found ->
			let tref = ref None in
			ctx.rec_cache <- (t,tref) :: ctx.rec_cache;
			let t = to_type ~tref ctx (apply_params td.t_params tl td.t_type) in
			ctx.rec_cache <- List.tl ctx.rec_cache;
			t
		) in
		(match td.t_path with
		| [], "Null" when not (is_nullable t) -> HNull t
		| _ -> t)
	| TLazy f ->
		to_type ?tref ctx (!f())
	| TFun (args, ret) ->
		HFun (List.map (fun (_,o,t) -> to_type ctx (if o then ctx.com.basic.tnull t else t)) args, to_type ctx ret)
	| TAnon a when (match !(a.a_status) with Statics _ | EnumStatics _ -> true | _ -> false) ->
		(match !(a.a_status) with
		| Statics c ->
			class_type ctx c (List.map snd c.cl_params) true
		| EnumStatics e ->
			enum_class ctx e
		| _ -> assert false)
	| TAnon a ->
		(try
			(* can't use physical comparison in PMap since addresses might change in GC compact,
				maybe add an uid to tanon if too slow ? *)
			List.assq a ctx.anons_cache
		with Not_found ->
			let vp = {
				vfields = [||];
				vindex = PMap.empty;
			} in
			let t = HVirtual vp in
			(match tref with
			| None -> ()
			| Some r -> r := Some t);
			ctx.anons_cache <- (a,t) :: ctx.anons_cache;
			let fields = PMap.fold (fun cf acc ->
				match cf.cf_kind with
				| Var _ when (match follow cf.cf_type with TAnon _ | TFun _ -> true | _ -> false) ->
					(*
						if it's another virtual or a method, it might not match our own (might be larger, or class)
					*)
					acc
				| Method _ ->
					acc
				| _ ->
					(cf.cf_name,alloc_string ctx cf.cf_name,to_type ctx cf.cf_type) :: acc
			) a.a_fields [] in
			if fields = [] then
				let t = HDyn in
				ctx.anons_cache <- (a,t) :: List.tl ctx.anons_cache;
				t
			else
				let fields = List.sort (fun (n1,_,_) (n2,_,_) -> compare n1 n2) fields in
				vp.vfields <- Array.of_list fields;
				Array.iteri (fun i (n,_,_) -> vp.vindex <- PMap.add n i vp.vindex) vp.vfields;
				t
		)
	| TDynamic _ ->
		HDyn
	| TEnum (e,_) ->
		enum_type ~tref ctx e
	| TInst ({ cl_path = ["hl";"types"],"NativeAbstract" },[TInst({ cl_kind = KExpr (EConst (String name),_) },_)]) ->
		HAbstract (name, alloc_string ctx name)
	| TInst (c,pl) ->
		(match c.cl_kind with
		| KTypeParameter tl ->
			let rec loop = function
				| [] -> HDyn
				| t :: tl ->
					match follow (apply_params c.cl_params pl t) with
					| TInst ({cl_interface=false},_) as t -> to_type ?tref ctx t
					| _ -> loop tl
			in
			loop tl
		| _ -> class_type ~tref ctx c pl false)
	| TAbstract (a,pl) ->
		if Meta.has Meta.CoreType a.a_meta then
			(match a.a_path with
			| [], "Void" -> HVoid
			| [], "Int" | [], "UInt" -> HI32
			| [], "Float" -> HF64
			| [], "Single" -> HF32
			| [], "Bool" -> HBool
			| [], "Dynamic" -> HDyn
			| [], "Class" ->
				class_type ctx ctx.base_class [] false
			| [], "Enum" ->
				class_type ctx ctx.base_type [] false
			| [], "EnumValue" -> HDyn
			| ["hl";"types"], "Ref" -> HRef (to_type ctx (List.hd pl))
			| ["hl";"types"], ("Bytes" | "BytesAccess") -> HBytes
			| ["hl";"types"], "Type" -> HType
			| ["hl";"types"], "UI16" -> HUI16
			| ["hl";"types"], "UI8" -> HUI8
			| ["hl";"types"], "NativeArray" -> HArray
			| _ -> failwith ("Unknown core type " ^ s_type_path a.a_path))
		else
			to_type ?tref ctx (Abstract.get_underlying_type a pl)

and resolve_class ctx c pl statics =
	let not_supported() =
		failwith ("Extern type not supported : " ^ s_type (print_context()) (TInst (c,pl)))
	in
	match c.cl_path, pl with
	| ([],"Array"), [t] ->
		if statics then ctx.array_impl.abase else array_class ctx (to_type ctx t)
	| ([],"Array"), [] ->
		assert false
	| _, _ when c.cl_extern ->
		not_supported()
	| _ ->
		c

and field_type ctx f p =
	match f with
	| FInstance (c,pl,f) | FClosure (Some (c,pl),f) ->
		let creal = resolve_class ctx c pl false in
		let rec loop c =
			try
				PMap.find f.cf_name c.cl_fields
			with Not_found ->
				match c.cl_super with
				| Some (csup,_) -> loop csup
				| None -> abort (s_type_path creal.cl_path ^ " is missing field " ^ f.cf_name) p
		in
		(loop creal).cf_type
	| FStatic (_,f) | FAnon f | FClosure (_,f) -> f.cf_type
	| FDynamic _ -> t_dynamic
	| FEnum (_,f) -> f.ef_type

and real_type ctx e =
	let rec loop e =
		match e.eexpr with
		| TField (_,f) ->
			let ft = field_type ctx f e.epos in
			(*
				Handle function variance:
				If we have type parameters which are function types, we need to keep the functions
				because we might need to insert a cast to coerce Void->Bool to Void->Dynamic for instance.
			*)
			(match ft, e.etype with
			| TFun (args,ret), TFun (args2,_) ->
				TFun (List.map2 (fun ((_,_,t) as a) ((_,_,t2) as a2) -> match t, t2 with TInst ({cl_kind=KTypeParameter _},_), TFun _ -> a2 | _ -> a) args args2, ret)
			| _ -> ft)
		| TLocal v -> v.v_type
		| TParenthesis e -> loop e
		| _ -> e.etype
	in
	to_type ctx (loop e)

and class_type ?(tref=None) ctx c pl statics =
	let c = if c.cl_extern then resolve_class ctx c pl statics else c in
	let key_path = (if statics then fst c.cl_path, "$" ^ snd c.cl_path else c.cl_path) in
	try
		PMap.find key_path ctx.cached_types
	with Not_found when c.cl_interface && not statics ->
		let vp = {
			vfields = [||];
			vindex = PMap.empty;
		} in
		let t = HVirtual vp in
		ctx.cached_types <- PMap.add c.cl_path t ctx.cached_types;
		let rec loop c =
			let fields = List.fold_left (fun acc (i,_) -> loop i @ acc) [] c.cl_implements in
			PMap.fold (fun cf acc -> (cf.cf_name,alloc_string ctx cf.cf_name,to_type ctx cf.cf_type) :: acc) c.cl_fields fields
		in
		let fields = loop c in
		vp.vfields <- Array.of_list fields;
		Array.iteri (fun i (n,_,_) -> vp.vindex <- PMap.add n i vp.vindex) vp.vfields;
		t
	| Not_found ->
		let pname = s_type_path key_path in
		let p = {
			pname = pname;
			pid = alloc_string ctx pname;
			psuper = None;
			pclassglobal = None;
			pproto = [||];
			pfields = [||];
			pindex = PMap.empty;
			pvirtuals = [||];
			pfunctions = PMap.empty;
			pnfields = -1;
		} in
		let t = HObj p in
		(match tref with
		| None -> ()
		| Some r -> r := Some t);
		ctx.cached_types <- PMap.add key_path t ctx.cached_types;
		if c.cl_path = ([],"Array") then assert false;
		if c == ctx.base_class then begin
			if statics then assert false;
			p.pnfields <- 1;
		end;
		let tsup = (match c.cl_super with
			| Some (csup,pl) when not statics -> Some (class_type ctx csup [] statics)
			| _ -> if statics then Some (class_type ctx ctx.base_class [] false) else None
		) in
		let start_field, virtuals = (match tsup with
			| None -> 0, [||]
			| Some (HObj psup) ->
				if psup.pnfields < 0 then assert false;
				p.psuper <- Some psup;
				p.pfunctions <- psup.pfunctions;
				psup.pnfields, psup.pvirtuals
			| _ -> assert false
		) in
		let fa = DynArray.create() and pa = DynArray.create() and virtuals = DynArray.of_array virtuals in
		let todo = ref [] in
		List.iter (fun f ->
			if is_extern_field f || (statics && f.cf_name = "__meta__") then () else
			match f.cf_kind with
			| Method m when m <> MethDynamic && not statics ->
				let g = alloc_fid ctx c f in
				p.pfunctions <- PMap.add f.cf_name g p.pfunctions;
				let virt = if List.exists (fun ff -> ff.cf_name = f.cf_name) c.cl_overrides then
					let vid = (try -(fst (get_index f.cf_name p))-1 with Not_found -> assert false) in
					DynArray.set virtuals vid g;
					Some vid
				else if is_overriden ctx c f then begin
					let vid = DynArray.length virtuals in
					DynArray.add virtuals g;
					p.pindex <- PMap.add f.cf_name (-vid-1,HVoid) p.pindex;
					Some vid
				end else
					None
				in
				DynArray.add pa { fname = f.cf_name; fid = alloc_string ctx f.cf_name; fmethod = g; fvirtual = virt; }
			| Method MethDynamic when List.exists (fun ff -> ff.cf_name = f.cf_name) c.cl_overrides ->
				()
			| _ ->
				let fid = DynArray.length fa in
				p.pindex <- PMap.add f.cf_name (fid + start_field, t) p.pindex;
				DynArray.add fa (f.cf_name, alloc_string ctx f.cf_name, HVoid);
				todo := (fun() ->
					let t = to_type ctx f.cf_type in
					p.pindex <- PMap.add f.cf_name (fid + start_field, t) p.pindex;
					Array.set p.pfields fid (f.cf_name, alloc_string ctx f.cf_name, t)
				) :: !todo;
		) (if statics then c.cl_ordered_statics else c.cl_ordered_fields);
		if not statics then (try
			let cf = PMap.find "toString" c.cl_fields in
			if List.memq cf c.cl_overrides || PMap.mem "__string" c.cl_fields || not (is_to_string cf.cf_type) then raise Not_found;
			DynArray.add pa { fname = "__string"; fid = alloc_string ctx "__string"; fmethod = alloc_fun_path ctx c.cl_path "__string"; fvirtual = None; }
		with Not_found ->
			());
		p.pnfields <- DynArray.length fa + start_field;
		p.pfields <- DynArray.to_array fa;
		p.pproto <- DynArray.to_array pa;
		p.pvirtuals <- DynArray.to_array virtuals;
		List.iter (fun f -> f()) !todo;
		if not statics && c != ctx.core_type && c != ctx.core_enum then p.pclassglobal <- Some (fst (class_global ctx (if statics then ctx.base_class else c)));
		t

and enum_type ?(tref=None) ctx e =
	try
		PMap.find e.e_path ctx.cached_types
	with Not_found ->
		let ename = s_type_path e.e_path in
		let et = {
			eglobal = None;
			ename = ename;
			eid = alloc_string ctx ename;
			efields = [||];
		} in
		let t = HEnum et in
		(match tref with
		| None -> ()
		| Some r -> r := Some t);
		ctx.cached_types <- PMap.add e.e_path t ctx.cached_types;
		et.efields <- Array.of_list (List.map (fun f ->
			let f = PMap.find f e.e_constrs in
			let args = (match f.ef_type with
				| TFun (args,_) -> Array.of_list (List.map (fun (_,_,t) -> to_type ctx t) args)
				| _ -> [||]
			) in
			(f.ef_name, alloc_string ctx f.ef_name, args)
		) e.e_names);
		let ct = enum_class ctx e in
		et.eglobal <- Some (alloc_global ctx (match ct with HObj o -> o.pname | _ -> assert false) ct);
		t

and enum_class ctx e =
	let cpath = (fst e.e_path,"$" ^ snd e.e_path) in
	try
		PMap.find cpath ctx.cached_types
	with Not_found ->
		let pname = s_type_path cpath in
		let p = {
			pname = pname;
			pid = alloc_string ctx pname;
			psuper = None;
			pclassglobal = None;
			pproto = [||];
			pfields = [||];
			pindex = PMap.empty;
			pvirtuals = [||];
			pfunctions = PMap.empty;
			pnfields = -1;
		} in
		let t = HObj p in
		ctx.cached_types <- PMap.add cpath t ctx.cached_types;
		p.psuper <- Some (match class_type ctx ctx.base_enum [] false with HObj o -> o | _ -> assert false);
		t

and alloc_fun_path ctx path name =
	lookup ctx.cfids (name, path) (fun() -> ())

and alloc_fid ctx c f =
	match f.cf_kind with
	| Var _ -> assert false
	| _ -> alloc_fun_path ctx c.cl_path f.cf_name

and alloc_eid ctx e f =
	alloc_fun_path ctx e.e_path f.ef_name

and alloc_function_name ctx f =
	alloc_fun_path ctx ([],"") f

and alloc_global ctx name t =
	lookup ctx.cglobals name (fun() -> t)

and class_global ?(resolve=true) ctx c =
	let static = c != ctx.base_class in
	let c = if resolve && is_array_type (HObj { null_proto with pname = s_type_path c.cl_path }) then ctx.array_impl.abase else c in
	let c = resolve_class ctx c (List.map snd c.cl_params) static in
	let t = class_type ctx c [] static in
	alloc_global ctx ("$" ^ s_type_path c.cl_path) t, t

let alloc_std ctx name args ret =
	let lib = "std" in
	(* different from :hlNative to prevent mismatch *)
	let nid = lookup ctx.cnatives ("$" ^ name ^ "@" ^ lib) (fun() ->
		let fid = alloc_fun_path ctx ([],"std") name in
		Hashtbl.add ctx.defined_funs fid ();
		(alloc_string ctx lib, alloc_string ctx name,HFun (args,ret),fid)
	) in
	let _,_,_,fid = DynArray.get ctx.cnatives.arr nid in
	fid

let alloc_reg ctx v =
	lookup ctx.m.mregs v.v_id (fun() -> to_type ctx v.v_type)

let alloc_tmp ctx t =
	let rid = DynArray.length ctx.m.mregs.arr in
	DynArray.add ctx.m.mregs.arr t;
	rid

let current_pos ctx =
	DynArray.length ctx.m.mops

let rtype ctx r =
	DynArray.get ctx.m.mregs.arr r

let op ctx o =
	DynArray.add ctx.m.mdebug ctx.m.mcurpos;
	DynArray.add ctx.m.mops o

let jump ctx f =
	let pos = current_pos ctx in
	op ctx (OJAlways (-1)); (* loop *)
	(fun() -> DynArray.set ctx.m.mops pos (f (current_pos ctx - pos - 1)))

let jump_back ctx =
	let pos = current_pos ctx in
	op ctx (OLabel 0);
	(fun() -> op ctx (OJAlways (pos - current_pos ctx - 1)))

let reg_int ctx v =
	let r = alloc_tmp ctx HI32 in
	op ctx (OInt (r,alloc_i32 ctx (Int32.of_int v)));
	r

let shl ctx idx v =
	if v = 0 then idx else
	let idx2 = alloc_tmp ctx HI32 in
	op ctx (OShl (idx2, idx, reg_int ctx v));
	idx2

let set_default ctx r =
	match rtype ctx r with
	| HUI8 | HUI16 | HI32 ->
		op ctx (OInt (r,alloc_i32 ctx 0l))
	| HF32 | HF64 ->
		op ctx (OFloat (r,alloc_float ctx 0.))
	| HBool ->
		op ctx (OBool (r, false))
	| HType ->
		op ctx (OType (r, HVoid))
	| _ ->
		op ctx (ONull r)

let read_mem ctx rdst bytes index t =
	match t with
	| HUI8 ->
		op ctx (OGetI8 (rdst,bytes,index))
	| HUI16 ->
		op ctx (OGetI16 (rdst,bytes,index))
	| HI32 ->
		op ctx (OGetI32 (rdst,bytes,index))
	| HF32 ->
		op ctx (OGetF32 (rdst,bytes,index))
	| HF64 ->
		op ctx (OGetF64 (rdst,bytes,index))
	| _ ->
		assert false

let write_mem ctx bytes index t r=
	match t with
	| HUI8 ->
		op ctx (OSetI8 (bytes,index,r))
	| HUI16 ->
		op ctx (OSetI16 (bytes,index,r))
	| HI32 ->
		op ctx (OSetI32 (bytes,index,r))
	| HF32 ->
		op ctx (OSetF32 (bytes,index,r))
	| HF64 ->
		op ctx (OSetF64 (bytes,index,r))
	| _ ->
		assert false

let common_type ctx e1 e2 for_eq p =
	let t1 = to_type ctx e1.etype in
	let t2 = to_type ctx e2.etype in
	let rec loop t1 t2 =
		if t1 == t2 then t1 else
		match t1, t2 with
		| HUI8, (HUI16 | HI32 | HF32 | HF64) -> t2
		| HUI16, (HI32 | HF32 | HF64) -> t2
		| HI32, HF32 -> t2 (* possible loss of precision *)
		| (HI32 | HF32), HF64 -> t2
		| (HUI8|HUI16|HI32|HF32|HF64), (HUI8|HUI16|HI32|HF32|HF64) -> t1
		| (HUI8|HUI16|HI32|HF32|HF64), (HNull t2) -> if for_eq then HNull (loop t1 t2) else loop t1 t2
		| (HNull t1), (HUI8|HUI16|HI32|HF32|HF64) -> if for_eq then HNull (loop t1 t2) else loop t1 t2
		| (HNull t1), (HNull t2) -> if for_eq then HNull (loop t1 t2) else loop t1 t2
		| HDyn, (HUI8|HUI16|HI32|HF32|HF64) -> HF64
		| (HUI8|HUI16|HI32|HF32|HF64), HDyn -> HF64
		| HDyn, _ -> HDyn
		| _, HDyn -> HDyn
		| _ when for_eq && safe_cast t1 t2 -> t2
		| _ when for_eq && safe_cast t2 t1 -> t1
		| HBool, HNull HBool when for_eq -> t2
		| HNull HBool, HBool when for_eq -> t1
		| HObj _, HVirtual _ | HVirtual _, HObj _ -> HDyn
		| _ ->
			abort ("Don't know how to compare " ^ tstr t1 ^ " and " ^ tstr t2) p
	in
	loop t1 t2

let captured_index ctx v =
	if not v.v_capture then None else try Some (PMap.find v.v_id ctx.m.mcaptured.c_map) with Not_found -> None

let before_return ctx =
	let rec loop i =
		if i > 0 then begin
			op ctx (OEndTrap false);
			loop (i - 1)
		end
	in
	loop ctx.m.mtrys

let type_value ctx t p =
	match t with
	| TClassDecl c ->
		let g, t = class_global ctx c in
		let r = alloc_tmp ctx t in
		op ctx (OGetGlobal (r, g));
		r
	| TAbstractDecl a ->
		let r = alloc_tmp ctx (class_type ctx ctx.base_type [] false) in
		(match a.a_path with
		| [], "Int" -> op ctx (OGetGlobal (r, alloc_global ctx "$Int" (rtype ctx r)))
		| [], "Float" -> op ctx (OGetGlobal (r, alloc_global ctx "$Float" (rtype ctx r)))
		| [], "Bool" -> op ctx (OGetGlobal (r, alloc_global ctx "$Bool" (rtype ctx r)))
		| [], "Class" -> op ctx (OGetGlobal (r, fst (class_global ctx ctx.base_class)))
		| [], "Enum" -> op ctx (OGetGlobal (r, fst (class_global ctx ctx.base_enum)))
		| [], "Dynamic" -> op ctx (OGetGlobal (r, alloc_global ctx "$Dynamic" (rtype ctx r)))
		| _ -> abort ("Unsupported type value " ^ s_type_path (t_path t)) p);
		r
	| TEnumDecl e ->
		let r = alloc_tmp ctx (enum_class ctx e) in
		let rt = rtype ctx r in
		op ctx (OGetGlobal (r, alloc_global ctx (match rt with HObj o -> o.pname | _ -> assert false) rt));
		r
	| TTypeDecl _ ->
		assert false

let rec eval_to ctx e (t:ttype) =
	let r = eval_expr ctx e in
	cast_to ctx r t e.epos

and cast_to ?(force=false) ctx (r:reg) (t:ttype) p =
	let rt = rtype ctx r in
	if safe_cast rt t then r else
	match rt, t with
	| _, HVoid ->
		alloc_tmp ctx HVoid
	| HVirtual _, HVirtual _ ->
		let tmp = alloc_tmp ctx HDyn in
		op ctx (OMov (tmp,r));
		cast_to ctx tmp t p
	| (HUI8 | HUI16 | HI32 | HF32 | HF64), (HF32 | HF64) ->
		let tmp = alloc_tmp ctx t in
		op ctx (OToSFloat (tmp, r));
		tmp
	| (HUI8 | HUI16 | HI32 | HF32 | HF64), (HUI8 | HUI16 | HI32) ->
		let tmp = alloc_tmp ctx t in
		op ctx (OToInt (tmp, r));
		tmp
	| (HUI8 | HUI16 | HI32), HObj { pname = "String" } ->
		let out = alloc_tmp ctx t in
		let len = alloc_tmp ctx HI32 in
		let lref = alloc_tmp ctx (HRef HI32) in
		let bytes = alloc_tmp ctx HBytes in
		op ctx (ORef (lref,len));
		op ctx (OCall2 (bytes,alloc_std ctx "itos" [HI32;HRef HI32] HBytes,cast_to ctx r HI32 p,lref));
		op ctx (OCall2 (out,alloc_fun_path ctx ([],"String") "__alloc__",bytes,len));
		out
	| (HF32 | HF64), HObj { pname = "String" } ->
		let out = alloc_tmp ctx t in
		let len = alloc_tmp ctx HI32 in
		let lref = alloc_tmp ctx (HRef HI32) in
		let bytes = alloc_tmp ctx HBytes in
		op ctx (ORef (lref,len));
		op ctx (OCall2 (bytes,alloc_std ctx "ftos" [HF64;HRef HI32] HBytes,cast_to ctx r HF64 p,lref));
		op ctx (OCall2 (out,alloc_fun_path ctx ([],"String") "__alloc__",bytes,len));
		out
	| _, HObj { pname = "String" } ->
		let out = alloc_tmp ctx t in
		let r = cast_to ctx r HDyn p in
		op ctx (OJNotNull (r,2));
		op ctx (ONull out);
		op ctx (OJAlways 1);
		op ctx (OCall1 (out,alloc_fun_path ctx ([],"Std") "string",r));
		out
	| (HObj _ | HDynObj | HDyn) , HVirtual _ ->
		let out = alloc_tmp ctx t in
		op ctx (OToVirtual (out,r));
		out
	| HDyn, _ ->
		let out = alloc_tmp ctx t in
		op ctx (OSafeCast (out, r));
		out
	| HNull rt, _ when t = rt ->
		let out = alloc_tmp ctx t in
		op ctx (OSafeCast (out, r));
		out
	| HVoid, HDyn ->
		let tmp = alloc_tmp ctx HDyn in
		op ctx (ONull tmp);
		tmp
	| _ , HDyn ->
		let tmp = alloc_tmp ctx HDyn in
		op ctx (OToDyn (tmp, r));
		tmp
	| _, HNull t when rt == t ->
		let tmp = alloc_tmp ctx (HNull t) in
		op ctx (OToDyn (tmp, r));
		tmp
	| HNull t1, HNull t2 ->
		let j = jump ctx (fun n -> OJNull (r,n)) in
		let rtmp = alloc_tmp ctx t1 in
		op ctx (OSafeCast (rtmp,r));
		let out = cast_to ctx rtmp t p in
		op ctx (OJAlways 1);
		j();
		op ctx (ONull out);
		out
	| (HUI8 | HUI16 | HI32 | HF32 | HF64), HNull ((HF32 | HF64) as t) ->
		let tmp = alloc_tmp ctx t in
		op ctx (OToSFloat (tmp, r));
		let r = alloc_tmp ctx (HNull t) in
		op ctx (OToDyn (r,tmp));
		r
	| (HUI8 | HUI16 | HI32 | HF32 | HF64), HNull ((HUI8 | HUI16 | HI32) as t) ->
		let tmp = alloc_tmp ctx t in
		op ctx (OToInt (tmp, r));
		let r = alloc_tmp ctx (HNull t) in
		op ctx (OToDyn (r,tmp));
		r
	| HNull ((HUI8 | HUI16 | HI32) as it), (HF32 | HF64) ->
		let i = alloc_tmp ctx it in
		op ctx (OSafeCast (i,r));
		let tmp = alloc_tmp ctx t in
		op ctx (OToSFloat (tmp, i));
		tmp
	| HNull ((HF32 | HF64) as it), (HUI8 | HUI16 | HI32) ->
		let i = alloc_tmp ctx it in
		op ctx (OSafeCast (i,r));
		let tmp = alloc_tmp ctx t in
		op ctx (OToInt (tmp, i));
		tmp
	| HFun (args1,ret1), HFun (args2, ret2) when List.length args1 = List.length args2 ->
		let fid = gen_method_wrapper ctx rt t p in
		let fr = alloc_tmp ctx t in
		op ctx (OJNotNull (r,2));
		op ctx (ONull fr);
		op ctx (OJAlways 1);
		op ctx (OInstanceClosure (fr,fid,r));
		fr
	| HObj _, HObj _ when is_array_type rt && is_array_type t ->
		let out = alloc_tmp ctx t in
		op ctx (OSafeCast (out, r));
		out
	| _ ->
		if force then
			let out = alloc_tmp ctx t in
			op ctx (OSafeCast (out, r));
			out
		else
			abort ("Don't know how to cast " ^ tstr rt ^ " to " ^ tstr t) p

and unsafe_cast_to ctx (r:reg) (t:ttype) p =
	let rt = rtype ctx r in
	if safe_cast rt t then
		r
	else
	match rt with
	| HFun _ ->
		cast_to ctx r t p
	| HDyn when is_array_type t ->
		cast_to ctx r t p
	| HDyn when (match t with HVirtual _ -> true | _ -> false) ->
		cast_to ctx r t p
	| HObj _ when is_array_type rt && is_array_type t ->
		cast_to ctx r t p
	| _ ->
		if is_dynamic (rtype ctx r) && is_dynamic t then
			let r2 = alloc_tmp ctx t in
			op ctx (OUnsafeCast (r2,r));
			r2
		else
			cast_to ~force:true ctx r t p

and object_access ctx eobj t f =
	match t with
	| HObj p ->
		(try
			let fid = fst (get_index f.cf_name p) in
			if f.cf_kind = Method MethNormal then
				AInstanceProto (eobj, -fid-1)
			else
				AInstanceField (eobj, fid)
		with Not_found ->
			ADynamic (eobj, alloc_string ctx f.cf_name))
	| HVirtual v ->
		(try
			let fid = PMap.find f.cf_name v.vindex in
			if f.cf_kind = Method MethNormal then
				AVirtualMethod (eobj, fid)
			else
				AInstanceField (eobj, fid)
		with Not_found ->
			ADynamic (eobj, alloc_string ctx f.cf_name))
	| HDyn ->
		ADynamic (eobj, alloc_string ctx f.cf_name)
	| _ ->
		abort ("Unsupported field access " ^ tstr t) eobj.epos

and get_access ctx e =
	match e.eexpr with
	| TField (ethis, a) ->
		(match a, follow ethis.etype with
		| FStatic (c,({ cf_kind = Var _ | Method MethDynamic } as f)), _ ->
			let g, t = class_global ctx c in
			AStaticVar (g, t, (match t with HObj o -> (try fst (get_index f.cf_name o) with Not_found -> assert false) | _ -> assert false))
		| FStatic (c,({ cf_kind = Method _ } as f)), _ ->
			AStaticFun (alloc_fid ctx c f)
		| FClosure (Some (cdef,pl), ({ cf_kind = Method m } as f)), TInst (c,_)
		| FInstance (cdef,pl,({ cf_kind = Method m } as f)), TInst (c,_) when m <> MethDynamic && not (c.cl_interface || (is_overriden ctx c f && ethis.eexpr <> TConst(TSuper))) ->
			(* cdef is the original definition, we want the last redefinition *)
			let rec loop c =
				if PMap.mem f.cf_name c.cl_fields then c else (match c.cl_super with None -> cdef | Some (c,_) -> loop c)
			in
			let last_def = loop c in
			AInstanceFun (ethis, alloc_fid ctx (resolve_class ctx last_def pl false) f)
		| (FInstance (cdef,pl,f) | FClosure (Some (cdef,pl), f)), _ ->
			let cdef, pl = if cdef.cl_interface then (match follow ethis.etype with TInst (c,pl) -> c,pl | _ -> assert false) else cdef,pl in
			object_access ctx ethis (class_type ctx cdef pl false) f
		| (FAnon f | FClosure(None,f)), _ ->
			object_access ctx ethis (to_type ctx ethis.etype) f
		| FDynamic name, _ ->
			ADynamic (ethis, alloc_string ctx name)
		| FEnum (e,ef), _ ->
			(match follow ef.ef_type with
			| TFun _ -> AEnum (e,ef.ef_index)
			| t -> AGlobal (alloc_global ctx (efield_name e ef) (to_type ctx t))))
	| TLocal v ->
		(match captured_index ctx v with
		| None -> ALocal (alloc_reg ctx v)
		| Some idx -> ACaptured idx)
	| TParenthesis e ->
		get_access ctx e
	| TArray (a,i) ->
		(match follow a.etype with
		| TInst({ cl_path = [],"Array" },[t]) ->
			let a = eval_null_check ctx a in
			let i = eval_to ctx i HI32 in
			let t = to_type ctx t in
			AArray (a,(t,t),i)
		| _ ->
			let a = eval_to ctx a (class_type ctx ctx.array_impl.adyn [] false) in
			op ctx (ONullCheck a);
			let i = eval_to ctx i HI32 in
			AArray (a,(HDyn,to_type ctx e.etype),i)
		)
	| _ ->
		ANone

and array_read ctx ra (at,vt) ridx p =
	match at with
	| HUI8 | HUI16 | HI32 | HF32 | HF64 ->
		(* check bounds *)
		let length = alloc_tmp ctx HI32 in
		op ctx (OField (length, ra, 0));
		let r = alloc_tmp ctx (match at with HUI8 | HUI16 -> HI32 | _ -> at) in
		let j = jump ctx (fun i -> OJULt (ridx,length,i)) in
		(match at with
		| HUI8 | HUI16 | HI32 ->
			op ctx (OInt (r,alloc_i32 ctx 0l));
		| HF32 | HF64 ->
			op ctx (OFloat (r,alloc_float ctx 0.));
		| _ ->
			assert false);
		let jend = jump ctx (fun i -> OJAlways i) in
		j();
		let hbytes = alloc_tmp ctx HBytes in
		op ctx (OField (hbytes, ra, 1));
		read_mem ctx r hbytes (shl ctx ridx (type_size_bits at)) at;
		jend();
		cast_to ctx r vt p
	| HDyn ->
		(* call getDyn *)
		let r = alloc_tmp ctx HDyn in
		op ctx (OCallMethod (r,0,[ra;ridx]));
		unsafe_cast_to ctx r vt p
	| _ ->
		(* check bounds *)
		let length = alloc_tmp ctx HI32 in
		op ctx (OField (length,ra,0));
		let r = alloc_tmp ctx vt in
		let j = jump ctx (fun i -> OJULt (ridx,length,i)) in
		set_default ctx r;
		let jend = jump ctx (fun i -> OJAlways i) in
		j();
		let tmp = alloc_tmp ctx HDyn in
		let harr = alloc_tmp ctx HArray in
		op ctx (OField (harr,ra,1));
		op ctx (OGetArray (tmp,harr,ridx));
		op ctx (OMov (r,unsafe_cast_to ctx tmp vt p));
		jend();
		r

and jump_expr ctx e jcond =
	match e.eexpr with
	| TParenthesis e ->
		jump_expr ctx e jcond
	| TUnop (Not,_,e) ->
		jump_expr ctx e (not jcond)
	| TBinop (OpEq,{ eexpr = TConst(TNull) },e) | TBinop (OpEq,e,{ eexpr = TConst(TNull) }) ->
		let r = eval_expr ctx e in
		jump ctx (fun i -> if jcond then OJNull (r,i) else OJNotNull (r,i))
	| TBinop (OpNotEq,{ eexpr = TConst(TNull) },e) | TBinop (OpNotEq,e,{ eexpr = TConst(TNull) }) ->
		let r = eval_expr ctx e in
		jump ctx (fun i -> if jcond then OJNotNull (r,i) else OJNull (r,i))
	| TBinop (OpEq | OpNotEq | OpGt | OpGte | OpLt | OpLte as jop, e1, e2) ->
		let t = common_type ctx e1 e2 (match jop with OpEq | OpNotEq -> true | _ -> false) e.epos in
		let r1 = eval_to ctx e1 t in
		let r2 = eval_to ctx e2 t in
		let unsigned = unsigned e1.etype && unsigned e2.etype in
		jump ctx (fun i ->
			let lt a b = if unsigned then OJULt (a,b,i) else OJSLt (a,b,i) in
			let gte a b = if unsigned then OJUGte (a,b,i) else OJSGte (a,b,i) in
			match jop with
			| OpEq -> if jcond then OJEq (r1,r2,i) else OJNotEq (r1,r2,i)
			| OpNotEq -> if jcond then OJNotEq (r1,r2,i) else OJEq (r1,r2,i)
			| OpGt -> if jcond then lt r2 r1 else gte r2 r1
			| OpGte -> if jcond then gte r1 r2 else lt r1 r2
			| OpLt -> if jcond then lt r1 r2 else gte r1 r2
			| OpLte -> if jcond then gte r2 r1 else lt r2 r1
			| _ -> assert false
		)
	| TBinop (OpBoolAnd, e1, e2) ->
		let j = jump_expr ctx e1 false in
		let j2 = jump_expr ctx e2 jcond in
		if jcond then j();
		(fun() -> if not jcond then j(); j2());
	| TBinop (OpBoolOr, e1, e2) ->
		let j = jump_expr ctx e1 true in
		let j2 = jump_expr ctx e2 jcond in
		if not jcond then j();
		(fun() -> if jcond then j(); j2());
	| _ ->
		let r = eval_to ctx e HBool in
		jump ctx (fun i -> if jcond then OJTrue (r,i) else OJFalse (r,i))

and eval_args ctx el t p =
	let rl = List.map2 (fun e t -> eval_to ctx e t) el (match t with HFun (args,_) -> args | HDyn -> List.map (fun _ -> HDyn) el | _ -> assert false) in
	set_curpos ctx p;
	rl

and eval_null_check ctx e =
	let r = eval_expr ctx e in
	(match e.eexpr with
	| TConst TThis | TConst TSuper -> ()
	| _ -> op ctx (ONullCheck r));
	r

and make_string ctx s p =
	let str, len = to_utf8 s p in
	let r = alloc_tmp ctx HBytes in
	let s = alloc_tmp ctx (to_type ctx ctx.com.basic.tstring) in
	op ctx (ONew s);
	op ctx (OString (r,alloc_string ctx str));
	op ctx (OSetField (s,0,r));
	op ctx (OSetField (s,1,reg_int ctx len));
	s

and eval_expr ctx e =
	set_curpos ctx e.epos;
	match e.eexpr with
	| TConst c ->
		(match c with
		| TInt i ->
			let r = alloc_tmp ctx HI32 in
			op ctx (OInt (r,alloc_i32 ctx i));
			r
		| TFloat f ->
			let r = alloc_tmp ctx HF64 in
			op ctx (OFloat (r,alloc_float ctx (float_of_string f)));
			r
		| TBool b ->
			let r = alloc_tmp ctx HBool in
			op ctx (OBool (r,b));
			r
		| TString s ->
			make_string ctx s e.epos
		| TThis | TSuper ->
			0 (* first reg *)
		| _ ->
			let r = alloc_tmp ctx (to_type ctx e.etype) in
			op ctx (ONull r);
			r)
	| TVar (v,e) ->
		(match e with
		| None -> ()
		| Some e ->
			match captured_index ctx v with
			| None ->
				let r = alloc_reg ctx v in
				let ri = eval_to ctx e (rtype ctx r) in
				op ctx (OMov (r,ri))
			| Some idx ->
				let ri = eval_to ctx e (to_type ctx v.v_type) in
				op ctx (OSetEnumField (ctx.m.mcaptreg, idx, ri));
		);
		alloc_tmp ctx HVoid
	| TLocal v ->
		cast_to ctx (match captured_index ctx v with
		| None ->
			(* we need to make a copy for cases such as (a - a++) *)
			let r = alloc_reg ctx v in
			let r2 = alloc_tmp ctx (rtype ctx r) in
			op ctx (OMov (r2, r));
			r2
		| Some idx ->
			let r = alloc_tmp ctx (to_type ctx v.v_type) in
			op ctx (OEnumField (r, ctx.m.mcaptreg, 0, idx));
			r) (to_type ctx e.etype) e.epos
	| TReturn None ->
		before_return ctx;
		let r = alloc_tmp ctx HVoid in
		op ctx (ORet r);
		r
	| TReturn (Some e) ->
		let r = eval_to ctx e ctx.m.mret in
		before_return ctx;
		op ctx (ORet r);
		alloc_tmp ctx HVoid
	| TParenthesis e ->
		eval_expr ctx e
	| TBlock el ->
		let rec loop = function
			| [e] -> eval_expr ctx e
			| [] -> alloc_tmp ctx HVoid
			| e :: l ->
				ignore(eval_expr ctx e);
				loop l
		in
		loop el
	| TCall ({ eexpr = TConst TSuper } as s, el) ->
		(match follow s.etype with
		| TInst (csup,_) ->
			(match csup.cl_constructor with
			| None -> assert false
			| Some f ->
				let r = alloc_tmp ctx HVoid in
				let el = eval_args ctx el (to_type ctx f.cf_type) e.epos in
				op ctx (OCallN (r, alloc_fid ctx csup f, 0 :: el));
				r
			)
		| _ -> assert false);
	| TCall ({ eexpr = TLocal v }, el) when v.v_name.[0] = '$' ->
		let invalid() = abort "Invalid native call" e.epos in
		(match v.v_name, el with
		| "$new", [{ eexpr = TTypeExpr (TClassDecl _) }] ->
			(match follow e.etype with
			| TInst (c,pl) ->
				let r = alloc_tmp ctx (class_type ctx c pl false) in
				op ctx (ONew r);
				r
			| _ ->
				invalid())
		| "$int", [{ eexpr = TBinop (OpDiv, e1, e2) }] when is_int (to_type ctx e1.etype) && is_int (to_type ctx e2.etype) ->
			let tmp = alloc_tmp ctx HI32 in
			op ctx (if unsigned e1.etype && unsigned e2.etype then OUDiv (tmp, eval_to ctx e1 HI32, eval_to ctx e2 HI32) else OSDiv (tmp, eval_to ctx e1 HI32, eval_to ctx e2 HI32));
			tmp
		| "$int", [e] ->
			let tmp = alloc_tmp ctx HI32 in
			op ctx (OToInt (tmp, eval_expr ctx e));
			tmp
		| "$bseti8", [b;pos;v] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = eval_to ctx v HI32 in
			op ctx (OSetI8 (b, pos, r));
			r
		| "$bseti32", [b;pos;v] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = eval_to ctx v HI32 in
			op ctx (OSetI32 (b, pos, r));
			r
		| "$bsetf32", [b;pos;v] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = eval_to ctx v HF32 in
			op ctx (OSetF32 (b, pos, r));
			r
		| "$bsetf64", [b;pos;v] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = eval_to ctx v HF64 in
			op ctx (OSetF64 (b, pos, r));
			r
		| "$bytes_sizebits", [eb] ->
			(match follow eb.etype with
			| TAbstract({a_path = ["hl";"types"],"BytesAccess"},[t]) ->
				reg_int ctx (match to_type ctx t with
				| HUI8 -> 0
				| HUI16 -> 1
				| HI32 -> 2
				| HF32 -> 2
				| HF64 -> 3
				| t -> abort ("Unsupported basic type " ^ tstr t) e.epos)
			| _ ->
				abort "Invalid BytesAccess" eb.epos);
		| "$bytes_nullvalue", [eb] ->
			(match follow eb.etype with
			| TAbstract({a_path = ["hl";"types"],"BytesAccess"},[t]) ->
				let t = to_type ctx t in
				let r = alloc_tmp ctx t in
				(match t with
				| HUI8 | HUI16 | HI32 ->
					op ctx (OInt (r,alloc_i32 ctx 0l))
				| HF32 | HF64 ->
					op ctx (OFloat (r, alloc_float ctx 0.))
				| t ->
					abort ("Unsupported basic type " ^ tstr t) e.epos);
				r
			| _ ->
				abort "Invalid BytesAccess" eb.epos);
		| "$bget", [eb;pos] ->
			(match follow eb.etype with
			| TAbstract({a_path = ["hl";"types"],"BytesAccess"},[t]) ->
				let b = eval_to ctx eb HBytes in
				let pos = eval_to ctx pos HI32 in
				let t = to_type ctx t in
				(match t with
				| HUI8 ->
					let r = alloc_tmp ctx HI32 in
					op ctx (OGetI8 (r, b, pos));
					r
				| HUI16 ->
					let r = alloc_tmp ctx HI32 in
					op ctx (OGetI16 (r, b, shl ctx pos 1));
					r
				| HI32 ->
					let r = alloc_tmp ctx HI32 in
					op ctx (OGetI32 (r, b, shl ctx pos 2));
					r
				| HF32 ->
					let r = alloc_tmp ctx HF32 in
					op ctx (OGetF32 (r, b, shl ctx pos 2));
					r
				| HF64 ->
					let r = alloc_tmp ctx HF64 in
					op ctx (OGetF64 (r, b, shl ctx pos 3));
					r
				| _ ->
					abort ("Unsupported basic type " ^ tstr t) e.epos)
			| _ ->
				abort "Invalid BytesAccess" eb.epos);
		| "$bset", [eb;pos;value] ->
			(match follow eb.etype with
			| TAbstract({a_path = ["hl";"types"],"BytesAccess"},[t]) ->
				let b = eval_to ctx eb HBytes in
				let pos = eval_to ctx pos HI32 in
				let t = to_type ctx t in
				(match t with
				| HUI8 ->
					let v = eval_to ctx value HI32 in
					op ctx (OSetI8 (b, pos, v));
					v
				| HUI16 ->
					let v = eval_to ctx value HI32 in
					op ctx (OSetI16 (b, shl ctx pos 1, v));
					v
				| HI32 ->
					let v = eval_to ctx value HI32 in
					op ctx (OSetI32 (b, shl ctx pos 2, v));
					v
				| HF32 ->
					let v = eval_to ctx value HF32 in
					op ctx (OSetF32 (b, shl ctx pos 2, v));
					v
				| HF64 ->
					let v = eval_to ctx value HF64 in
					op ctx (OSetF64 (b, shl ctx pos 3, v));
					v
				| _ ->
					abort ("Unsupported basic type " ^ tstr t) e.epos)
			| _ ->
				abort "Invalid BytesAccess" eb.epos);
		| "$bgeti8", [b;pos] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = alloc_tmp ctx HI32 in
			op ctx (OGetI8 (r, b, pos));
			r
		| "$bgeti32", [b;pos] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = alloc_tmp ctx HI32 in
			op ctx (OGetI32 (r, b, pos));
			r
		| "$bgetf32", [b;pos] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = alloc_tmp ctx HF32 in
			op ctx (OGetF32 (r, b, pos));
			r
		| "$bgetf64", [b;pos] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = alloc_tmp ctx HF64 in
			op ctx (OGetF64 (r, b, pos));
			r
		| "$asize", [e] ->
			let r = alloc_tmp ctx HI32 in
			op ctx (OArraySize (r, eval_to ctx e HArray));
			r
		| "$aalloc", [esize] ->
			let et = (match follow e.etype with TAbstract ({ a_path = ["hl";"types"],"NativeArray" },[t]) -> to_type ctx t | _ -> invalid()) in
			let a = alloc_tmp ctx HArray in
			let rt = alloc_tmp ctx HType in
			op ctx (OType (rt,et));
			let size = eval_to ctx esize HI32 in
			op ctx (OCall2 (a,alloc_std ctx "alloc_array" [HType;HI32] HArray,rt,size));
			a
		| "$aget", [a; pos] ->
			(*
				read/write on arrays are unsafe : the type of NativeArray needs to be correcly set.
			*)
			let at = (match follow a.etype with TAbstract ({ a_path = ["hl";"types"],"NativeArray" },[t]) -> to_type ctx t | _ -> invalid()) in
			let arr = eval_to ctx a HArray in
			let pos = eval_to ctx pos HI32 in
			let r = alloc_tmp ctx at in
			op ctx (OGetArray (r, arr, pos));
			cast_to ctx r (to_type ctx e.etype) e.epos
		| "$aset", [a; pos; value] ->
			let et = (match follow a.etype with TAbstract ({ a_path = ["hl";"types"],"NativeArray" },[t]) -> to_type ctx t | _ -> invalid()) in
			let arr = eval_to ctx a HArray in
			let pos = eval_to ctx pos HI32 in
			let r = eval_to ctx value et in
			op ctx (OSetArray (arr, pos, r));
			r
		| "$ref", [v] ->
			(match v.eexpr with
			| TLocal v ->
				let r = alloc_tmp ctx (to_type ctx e.etype) in
				let rv = (match rtype ctx r with HRef t -> alloc_reg ctx v | _ -> invalid()) in
				op ctx (ORef (r,rv));
				r
			| _ ->
				abort "Ref should be a local variable" v.epos)
		| "$setref", [e1;e2] ->
			let rec loop e = match e.eexpr with
				| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) -> loop e1
				| TLocal v -> v
				| _ -> invalid()
			in
			let v = loop e1 in
			let r = alloc_reg ctx v in
			let rv = eval_to ctx e2 (match rtype ctx r with HRef t -> t | _ -> invalid()) in
			op ctx (OSetref (r,rv));
			r
		| "$unref", [e1] ->
			let rec loop e = match e.eexpr with
				| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) -> loop e1
				| TLocal v -> v
				| _ -> invalid()
			in
			let v = loop e1 in
			let r = alloc_reg ctx v in
			let out = alloc_tmp ctx (match rtype ctx r with HRef t -> t | _ -> invalid()) in
			op ctx (OUnref (out,r));
			out
		| "$ttype", [v] ->
			let r = alloc_tmp ctx HType in
			op ctx (OType (r,to_type ctx v.etype));
			r
		| "$tdyntype", [v] ->
			let r = alloc_tmp ctx HType in
			op ctx (OGetType (r,eval_to ctx v HDyn));
			r
		| "$tkind", [v] ->
			let r = alloc_tmp ctx HI32 in
			op ctx (OGetTID (r,eval_to ctx v HType));
			r
		| "$dump", [v] ->
			op ctx (ODump (eval_expr ctx v));
			alloc_tmp ctx HVoid
		| "$resources", [] ->
			let tdef = (try List.find (fun t -> (t_infos t).mt_path = (["haxe";"_Resource"],"ResourceContent")) ctx.com.types with Not_found -> assert false) in
			let t = class_type ctx (match tdef with TClassDecl c -> c | _ -> assert false) [] false in
			let arr = alloc_tmp ctx HArray in
			let rt = alloc_tmp ctx HType in
			op ctx (OType (rt,t));
			let res = Hashtbl.fold (fun k v acc -> (k,v) :: acc) ctx.com.resources [] in
			let size = reg_int ctx (List.length res) in
			op ctx (OCall2 (arr,alloc_std ctx "alloc_array" [HType;HI32] HArray,rt,size));
			let ro = alloc_tmp ctx t in
			let rb = alloc_tmp ctx HBytes in
			let ridx = reg_int ctx 0 in
			let has_len = (match t with HObj p -> PMap.mem "dataLen" p.pindex | _ -> assert false) in
			list_iteri (fun i (k,v) ->
				op ctx (ONew ro);
				op ctx (OString (rb,alloc_string ctx k));
				op ctx (OSetField (ro,0,rb));
				op ctx (OBytes (rb,alloc_string ctx (v ^ "\x00"))); (* add a \x00 to prevent clashing with existing string *)
				op ctx (OSetField (ro,1,rb));
				if has_len then op ctx (OSetField (ro,2,reg_int ctx (String.length v)));
				op ctx (OSetArray (arr,ridx,ro));
				op ctx (OIncr ridx);
			) res;
			arr
		| "$rethrow", [v] ->
			let r = alloc_tmp ctx HVoid in
			op ctx (ORethrow (eval_to ctx v HDyn));
			r
		| "$allTypes", [] ->
			let r = alloc_tmp ctx (to_type ctx e.etype) in
			op ctx (OGetGlobal (r, alloc_global ctx "__types__" (rtype ctx r)));
			r
		| "$allTypes", [v] ->
			let v = eval_expr ctx v in
			op ctx (OSetGlobal (alloc_global ctx "__types__" (rtype ctx v), v));
			v
		| "$hash", [v] ->
			(match v.eexpr with
			| TConst (TString str) ->
				let r = alloc_tmp ctx HI32 in
				op ctx (OInt (r,alloc_i32 ctx (hl_hash str)));
				r
			| _ -> abort "Constant string required" v.epos)
		| "$enumIndex", [v] ->
			let r = alloc_tmp ctx HI32 in
			let re = eval_expr ctx v in
			op ctx (ONullCheck re);
			op ctx (OEnumIndex (r,re));
			r
		| _ ->
			abort ("Unknown native call " ^ v.v_name) e.epos)
	| TCall (ec,args) ->
		let tfun = real_type ctx ec in
		let el() = eval_args ctx args tfun e.epos in
		let ret = alloc_tmp ctx (match tfun with HFun (_,r) -> r | _ -> HDyn) in
		let def_ret = ref None in
		(match get_access ctx ec with
		| AStaticFun f ->
			(match el() with
			| [] -> op ctx (OCall0 (ret, f))
			| [a] -> op ctx (OCall1 (ret, f, a))
			| [a;b] -> op ctx (OCall2 (ret, f, a, b))
			| [a;b;c] -> op ctx (OCall3 (ret, f, a, b, c))
			| [a;b;c;d] -> op ctx (OCall4 (ret, f, a, b, c, d))
			| el -> op ctx (OCallN (ret, f, el)));
		| AInstanceFun (ethis, f) ->
			let el = eval_null_check ctx ethis :: el() in
			(match el with
			| [a] -> op ctx (OCall1 (ret, f, a))
			| [a;b] -> op ctx (OCall2 (ret, f, a, b))
			| [a;b;c] -> op ctx (OCall3 (ret, f, a, b, c))
			| [a;b;c;d] -> op ctx (OCall4 (ret, f, a, b, c, d))
			| _ -> op ctx (OCallN (ret, f, el)));
		| AInstanceProto ({ eexpr = TConst TThis }, fid) ->
			op ctx (OCallThis (ret, fid, el()))
		| AInstanceProto (ethis, fid) | AVirtualMethod (ethis, fid) ->
			let el = eval_null_check ctx ethis :: el() in
			op ctx (OCallMethod (ret, fid, el))
		| AEnum (_,index) ->
			op ctx (OMakeEnum (ret, index, el()))
		| AArray (a,t,idx) ->
			let r = array_read ctx a t idx ec.epos in
			op ctx (ONullCheck r);
			op ctx (OCallClosure (ret, r, el())); (* if it's a value, it's a closure *)
		| _ ->
			let r = eval_null_check ctx ec in
			(* don't use real_type here *)
			let tfun = to_type ctx ec.etype in
			let el() = eval_args ctx args tfun e.epos in
			let ret = alloc_tmp ctx (match tfun with HFun (_,r) -> r | _ -> HDyn) in
			op ctx (OCallClosure (ret, r, el())); (* if it's a value, it's a closure *)
			def_ret := Some (unsafe_cast_to ctx ret (to_type ctx e.etype) e.epos);
		);
		(match !def_ret with None -> unsafe_cast_to ctx ret (to_type ctx e.etype) e.epos | Some r -> r)
	| TField (ec,FInstance({ cl_path = [],"Array" },[t],{ cf_name = "length" })) when to_type ctx t = HDyn ->
		let r = alloc_tmp ctx HI32 in
		op ctx (OCall1 (r,alloc_fun_path ctx (["hl";"types"],"ArrayDyn") "get_length", eval_null_check ctx ec));
		r
	| TField (ec,a) ->
		let r = alloc_tmp ctx (to_type ctx (field_type ctx a e.epos)) in
		(match get_access ctx e with
		| AGlobal g ->
			op ctx (OGetGlobal (r,g));
		| AStaticVar (g,t,fid) ->
			let o = alloc_tmp ctx t in
			op ctx (OGetGlobal (o,g));
			op ctx (OField (r,o,fid));
		| AStaticFun f ->
			op ctx (OStaticClosure (r,f));
		| AInstanceFun (ethis, f) ->
			op ctx (OInstanceClosure (r, f, eval_null_check ctx ethis))
		| AInstanceField (ethis,fid) ->
			let robj = eval_null_check ctx ethis in
			op ctx (match ethis.eexpr with TConst TThis -> OGetThis (r,fid) | _ -> OField (r,robj,fid));
		| AInstanceProto (ethis,fid) | AVirtualMethod (ethis, fid) ->
			let robj = eval_null_check ctx ethis in
			op ctx (OVirtualClosure (r,robj,fid));
		| ADynamic (ethis, f) ->
			let robj = eval_null_check ctx ethis in
			op ctx (ODynGet (r,robj,f))
		| AEnum (en,index) ->
			let cur_fid = DynArray.length ctx.cfids.arr in
			let name = List.nth en.e_names index in
			let fid = alloc_fun_path ctx en.e_path name in
			if fid = cur_fid then begin
				let ef = PMap.find name en.e_constrs in
				let eargs, et = (match follow ef.ef_type with TFun (args,ret) -> args, ret | _ -> assert false) in
				let ct = ctx.com.basic in
				let p = ef.ef_pos in
				let eargs = List.map (fun (n,o,t) -> alloc_var n t en.e_pos, if o then Some TNull else None) eargs in
				let ecall = mk (TCall (e,List.map (fun (v,_) -> mk (TLocal v) v.v_type p) eargs)) et p in
				let f = {
					tf_args = eargs;
					tf_type = et;
					tf_expr = mk (TReturn (Some ecall)) ct.tvoid p;
				} in
				ignore(make_fun ctx ("","") fid f None None);
			end;
			op ctx (OStaticClosure (r,fid));
		| ANone | ALocal _ | AArray _ | ACaptured _ ->
			abort "Invalid access" e.epos);
		unsafe_cast_to ctx r (to_type ctx e.etype) e.epos
	| TObjectDecl fl ->
		(match to_type ctx e.etype with
		| HVirtual vp as t when Array.length vp.vfields = List.length fl && not (List.exists (fun (s,e) -> s = "toString" && is_to_string e.etype) fl)  ->
			let r = alloc_tmp ctx t in
			op ctx (ONew r);
			List.iter (fun (s,ev) ->
				let fidx = (try PMap.find s vp.vindex with Not_found -> assert false) in
				let _, _, ft = vp.vfields.(fidx) in
				let v = eval_to ctx ev ft in
				op ctx (OSetField (r,fidx,v));
			) fl;
			r
		| _ ->
			let r = alloc_tmp ctx HDynObj in
			op ctx (ONew r);
			let a = (match follow e.etype with TAnon a -> Some a | t -> if t == t_dynamic then None else assert false) in
			List.iter (fun (s,ev) ->
				let ft = (try (match a with None -> raise Not_found | Some a -> PMap.find s a.a_fields).cf_type with Not_found -> ev.etype) in
				let v = eval_to ctx ev (to_type ctx ft) in
				op ctx (ODynSet (r,alloc_string ctx s,v));
				if s = "toString" && is_to_string ev.etype then begin
					let f = alloc_tmp ctx (HFun ([],HBytes)) in
					op ctx (OInstanceClosure (f, alloc_fun_path ctx ([],"String") "call_toString", r));
					op ctx (ODynSet (r,alloc_string ctx "__string",f));
				end;
			) fl;
			cast_to ctx r (to_type ctx e.etype) e.epos)
	| TNew (c,pl,el) ->
		let c = resolve_class ctx c pl false in
		let r = alloc_tmp ctx (class_type ctx c pl false) in
		op ctx (ONew r);
		(match c.cl_constructor with
		| None -> ()
		| Some { cf_expr = None } -> abort (s_type_path c.cl_path ^ " does not have a constructor") e.epos
		| Some ({ cf_expr = Some cexpr } as constr) ->
			let rl = eval_args ctx el (to_type ctx cexpr.etype) e.epos in
			let ret = alloc_tmp ctx HVoid in
			let g = alloc_fid ctx c constr in
			op ctx (match rl with
			| [] -> OCall1 (ret,g,r)
			| [a] -> OCall2 (ret,g,r,a)
			| [a;b] -> OCall3 (ret,g,r,a,b)
			| [a;b;c] -> OCall4 (ret,g,r,a,b,c)
			| _ -> OCallN (ret,g,r :: rl));
		);
		r
	| TIf (cond,eif,eelse) ->
		let t = to_type ctx e.etype in
		let out = alloc_tmp ctx t in
		let j = jump_expr ctx cond false in
		if t = HVoid then ignore(eval_expr ctx eif) else op ctx (OMov (out,eval_to ctx eif t));
		(match eelse with
		| None -> j()
		| Some e ->
			let jexit = jump ctx (fun i -> OJAlways i) in
			j();
			if t = HVoid then ignore(eval_expr ctx e) else op ctx (OMov (out,eval_to ctx e t));
			jexit());
		out
	| TBinop (bop, e1, e2) ->
		let is_unsigned() = unsigned e1.etype && unsigned e2.etype in
		let boolop r f =
			let j = jump ctx f in
			op ctx (OBool (r,false));
			op ctx (OJAlways 1);
			j();
			op ctx (OBool (r, true));
		in
		let binop r a b =
			let rec loop bop =
				match bop with
				| OpLte -> boolop r (fun d -> if is_unsigned() then OJUGte (b,a,d) else OJSLte (a,b,d))
				| OpGt -> boolop r (fun d -> if is_unsigned() then OJULt (b,a,d) else OJSGt (a,b,d))
				| OpGte -> boolop r (fun d -> if is_unsigned() then OJUGte (a,b,d) else OJSGte (a,b,d))
				| OpLt -> boolop r (fun d -> if is_unsigned() then OJULt (a,b,d) else OJSLt (a,b,d))
				| OpEq -> boolop r (fun d -> OJEq (a,b,d))
				| OpNotEq -> boolop r (fun d -> OJNotEq (a,b,d))
				| OpAdd ->
					(match rtype ctx r with
					| HUI8 | HUI16 | HI32 | HF32 | HF64 ->
						op ctx (OAdd (r,a,b))
					| HObj { pname = "String" } ->
						op ctx (OCall2 (r,alloc_fun_path ctx ([],"String") "__add__",a,b))
					| HDyn ->
						op ctx (OCall2 (r,alloc_fun_path ctx ([],"Std") "__add__",a,b))
					| t ->
						abort ("Cannot add " ^ tstr t) e.epos)
				| OpSub | OpMult | OpMod | OpDiv ->
					(match rtype ctx r with
					| HUI8 | HUI16 | HI32 | HF32 | HF64 ->
						(match bop with
						| OpSub -> op ctx (OSub (r,a,b))
						| OpMult -> op ctx (OMul (r,a,b))
						| OpMod -> op ctx (if is_unsigned() then OUMod (r,a,b) else OSMod (r,a,b))
						| OpDiv -> op ctx (OSDiv (r,a,b)) (* don't use UDiv since both operands are float already *)
						| _ -> assert false)
					| _ ->
						assert false)
				| OpShl | OpShr | OpUShr | OpAnd | OpOr | OpXor ->
					(match rtype ctx r with
					| HUI8 | HUI16 | HI32 ->
						(match bop with
						| OpShl -> op ctx (OShl (r,a,b))
						| OpShr -> op ctx (if unsigned e1.etype then OUShr (r,a,b) else OSShr (r,a,b))
						| OpUShr -> op ctx (OUShr (r,a,b))
						| OpAnd -> op ctx (OAnd (r,a,b))
						| OpOr -> op ctx (OOr (r,a,b))
						| OpXor -> op ctx (OXor (r,a,b))
						| _ -> ())
					| _ ->
						assert false)
				| OpAssignOp bop ->
					loop bop
				| _ ->
					assert false
			in
			loop bop
		in
		(match bop with
		| OpLte | OpGt | OpGte | OpLt ->
			let r = alloc_tmp ctx HBool in
			let t = common_type ctx e1 e2 false e.epos in
			let a = eval_to ctx e1 t in
			let b = eval_to ctx e2 t in
			binop r a b;
			r
		| OpEq | OpNotEq ->
			let r = alloc_tmp ctx HBool in
			let t = common_type ctx e1 e2 true e.epos in
			let a = eval_to ctx e1 t in
			let b = eval_to ctx e2 t in
			binop r a b;
			r
		| OpAdd | OpSub | OpMult | OpDiv | OpMod | OpShl | OpShr | OpUShr | OpAnd | OpOr | OpXor ->
			let t = (match to_type ctx e.etype with HNull t -> t | t -> t) in
			let r = alloc_tmp ctx t in
			let a = eval_to ctx e1 t in
			let b = eval_to ctx e2 t in
			binop r a b;
			r
		| OpAssign ->
			let value() = eval_to ctx e2 (real_type ctx e1) in
			(match get_access ctx e1 with
			| AGlobal g ->
				let r = value() in
				op ctx (OSetGlobal (g,r));
				r
			| AStaticVar (g,t,fid) ->
				let r = value() in
				let o = alloc_tmp ctx t in
				op ctx (OGetGlobal (o, g));
				op ctx (OSetField (o, fid, r));
				r
			| AInstanceField ({ eexpr = TConst TThis }, fid) ->
				let r = value() in
				op ctx (OSetThis (fid,r));
				r
			| AInstanceField (ethis, fid) ->
				let rthis = eval_null_check ctx ethis in
				let r = value() in
				op ctx (OSetField (rthis, fid, r));
				r
			| ALocal l ->
				let r = value() in
				op ctx (OMov (l, r));
				r
			| AArray (ra,(at,vt),ridx) ->
				let v = cast_to ctx (value()) (match at with HUI16 | HUI8 -> HI32 | _ -> at) e.epos in
				(* bounds check against length *)
				(match at with
				| HDyn ->
					(* call setDyn() *)
					op ctx (OCallMethod (alloc_tmp ctx HVoid,1,[ra;ridx;cast_to ctx v (if is_dynamic at then at else HDyn) e.epos]));
				| _ ->
					let len = alloc_tmp ctx HI32 in
					op ctx (OField (len,ra,0)); (* length *)
					let j = jump ctx (fun i -> OJULt (ridx,len,i)) in
					op ctx (OCall2 (alloc_tmp ctx HVoid, alloc_fun_path ctx (array_class ctx at).cl_path "__expand", ra, ridx));
					j();
					match at with
					| HI32 | HF64 | HUI16 | HF32 ->
						let b = alloc_tmp ctx HBytes in
						op ctx (OField (b,ra,1));
						write_mem ctx b (shl ctx ridx (type_size_bits at)) at v
					| _ ->
						let arr = alloc_tmp ctx HArray in
						op ctx (OField (arr,ra,1));
						op ctx (OSetArray (arr,ridx,cast_to ctx v (if is_dynamic at then at else HDyn) e.epos))
				);
				v
			| ADynamic (ethis,f) ->
				let obj = eval_null_check ctx ethis in
				let r = eval_expr ctx e2 in
				op ctx (ODynSet (obj,f,r));
				r
			| ACaptured index ->
				let r = value() in
				op ctx (OSetEnumField (ctx.m.mcaptreg,index,r));
				r
			| AEnum _ | ANone | AInstanceFun _ | AInstanceProto _ | AStaticFun _ | AVirtualMethod _ ->
				assert false)
		| OpBoolOr ->
			let r = alloc_tmp ctx HBool in
			let j = jump_expr ctx e1 true in
			let j2 = jump_expr ctx e2 true in
			op ctx (OBool (r,false));
			let jend = jump ctx (fun b -> OJAlways b) in
			j();
			j2();
			op ctx (OBool (r,true));
			jend();
			r
		| OpBoolAnd ->
			let r = alloc_tmp ctx HBool in
			let j = jump_expr ctx e1 false in
			let j2 = jump_expr ctx e2 false in
			op ctx (OBool (r,true));
			let jend = jump ctx (fun b -> OJAlways b) in
			j();
			j2();
			op ctx (OBool (r,false));
			jend();
			r
		| OpAssignOp bop ->
			(match get_access ctx e1 with
			| ALocal l ->
				let r = eval_to ctx { e with eexpr = TBinop (bop,e1,e2) } (to_type ctx e1.etype) in
				op ctx (OMov (l, r));
				r
			| acc ->
				gen_assign_op ctx acc e1 (fun r ->
					let b = eval_to ctx e2 (rtype ctx r) in
					binop r r b;
					r))
		| OpInterval | OpArrow ->
			assert false)
	| TUnop (Not,_,v) ->
		let tmp = alloc_tmp ctx HBool in
		let r = eval_to ctx v HBool in
		op ctx (ONot (tmp,r));
		tmp
	| TUnop (Neg,_,v) ->
		let t = to_type ctx e.etype in
		let tmp = alloc_tmp ctx t in
		let r = eval_to ctx v t in
		op ctx (ONeg (tmp,r));
		tmp
	| TUnop (NegBits,_,v) ->
		let t = to_type ctx e.etype in
		let tmp = alloc_tmp ctx t in
		let r = eval_to ctx v t in
		let mask = (match t with
			| HUI8 -> 0xFFl
			| HUI16 -> 0xFFFFl
			| HI32 -> 0xFFFFFFFFl
			| _ -> abort (tstr t) e.epos
		) in
		let r2 = alloc_tmp ctx t in
		op ctx (OInt (r2,alloc_i32 ctx mask));
		op ctx (OXor (tmp,r,r2));
		tmp
	| TUnop (Increment|Decrement as uop,fix,v) ->
		let rec unop r =
			match rtype ctx r with
			| HUI8 | HUI16 | HI32 ->
				if uop = Increment then op ctx (OIncr r) else op ctx (ODecr r)
			| HF32 | HF64 as t ->
				let tmp = alloc_tmp ctx t in
				op ctx (OFloat (tmp,alloc_float ctx 1.));
				if uop = Increment then op ctx (OAdd (r,r,tmp)) else op ctx (OSub (r,r,tmp))
			| HNull (HUI8 | HUI16 | HI32 | HF32 | HF64 as t) ->
				let tmp = alloc_tmp ctx t in
				op ctx (OSafeCast (tmp,r));
				unop tmp;
				op ctx (OToDyn (r,tmp));
			| _ ->
				assert false
		in
		(match get_access ctx v, fix with
		| ALocal r, Prefix ->
			unop r;
			r
		| ALocal r, Postfix ->
			let r2 = alloc_tmp ctx (rtype ctx r) in
			op ctx (OMov (r2,r));
			unop r;
			r2
		| acc, _ ->
			let ret = ref 0 in
			ignore(gen_assign_op ctx acc v (fun r ->
				if fix = Prefix then ret := r else begin
					let tmp = alloc_tmp ctx (rtype ctx r) in
					op ctx (OMov (tmp, r));
					ret := tmp;
				end;
				unop r;
				r)
			);
			!ret)
	| TFunction f ->
		let fid = alloc_function_name ctx ("function#" ^ string_of_int (DynArray.length ctx.cfids.arr)) in
		let capt = make_fun ctx ("","") fid f None (Some ctx.m.mcaptured) in
		let r = alloc_tmp ctx (to_type ctx e.etype) in
		if capt == ctx.m.mcaptured then
			op ctx (OInstanceClosure (r, fid, ctx.m.mcaptreg))
		else if Array.length capt.c_vars > 0 then
			let env = alloc_tmp ctx capt.c_type in
			op ctx (OEnumAlloc (env,0));
			Array.iteri (fun i v ->
				let r = (match captured_index ctx v with
				| None -> alloc_reg ctx v
				| Some idx ->
					let r = alloc_tmp ctx (to_type ctx v.v_type) in
					op ctx (OEnumField (r,ctx.m.mcaptreg,0,idx));
					r
				) in
				op ctx (OSetEnumField (env,i,r));
			) capt.c_vars;
			op ctx (OInstanceClosure (r, fid, env))
		else
			op ctx (OStaticClosure (r, fid));
		r
	| TThrow v ->
		op ctx (OThrow (eval_to ctx v HDyn));
		alloc_tmp ctx HVoid
	| TWhile (cond,eloop,NormalWhile) ->
		let oldb = ctx.m.mbreaks and oldc = ctx.m.mcontinues in
		ctx.m.mbreaks <- [];
		ctx.m.mcontinues <- [];
		let continue_pos = current_pos ctx in
		let ret = jump_back ctx in
		let j = jump_expr ctx cond false in
		ignore(eval_expr ctx eloop);
		ret();
		j();
		List.iter (fun f -> f (current_pos ctx)) ctx.m.mbreaks;
		List.iter (fun f -> f continue_pos) ctx.m.mcontinues;
		ctx.m.mbreaks <- oldb;
		ctx.m.mcontinues <- oldc;
		alloc_tmp ctx HVoid
	| TWhile (cond,eloop,DoWhile) ->
		let oldb = ctx.m.mbreaks and oldc = ctx.m.mcontinues in
		ctx.m.mbreaks <- [];
		ctx.m.mcontinues <- [];
		let start = jump ctx (fun p -> OJAlways p) in
		let continue_pos = current_pos ctx in
		let ret = jump_back ctx in
		let j = jump_expr ctx cond false in
		start();
		ignore(eval_expr ctx eloop);
		ret();
		j();
		List.iter (fun f -> f (current_pos ctx)) ctx.m.mbreaks;
		List.iter (fun f -> f continue_pos) ctx.m.mcontinues;
		ctx.m.mbreaks <- oldb;
		ctx.m.mcontinues <- oldc;
		alloc_tmp ctx HVoid
	| TCast (v,None) ->
		let t = to_type ctx e.etype in
		let rv = eval_expr ctx v in
		(match t with
		| HF32 | HF64 when unsigned v.etype ->
			let r = alloc_tmp ctx t in
			op ctx (OToUFloat (r,rv));
			r
		| _ ->
			cast_to ~force:true ctx rv t e.epos)
	| TArrayDecl el ->
		let r = alloc_tmp ctx (to_type ctx e.etype) in
		let et = (match follow e.etype with TInst (_,[t]) -> to_type ctx t | _ -> assert false) in
		(match et with
		| HI32 ->
			let b = alloc_tmp ctx HBytes in
			let size = reg_int ctx ((List.length el) * 4) in
			op ctx (OCall1 (b,alloc_std ctx "alloc_bytes" [HI32] HBytes,size));
			list_iteri (fun i e ->
				let r = eval_to ctx e HI32 in
				op ctx (OSetI32 (b,reg_int ctx (i * 4),r));
			) el;
			op ctx (OCall2 (r, alloc_fun_path ctx (["hl";"types"],"ArrayBase") "allocI32", b, reg_int ctx (List.length el)));
		| HUI16 ->
			let b = alloc_tmp ctx HBytes in
			let size = reg_int ctx ((List.length el) * 2) in
			op ctx (OCall1 (b,alloc_std ctx "alloc_bytes" [HI32] HBytes,size));
			list_iteri (fun i e ->
				let r = eval_to ctx e HI32 in
				op ctx (OSetI16 (b,reg_int ctx (i * 2),r));
			) el;
			op ctx (OCall2 (r, alloc_fun_path ctx (["hl";"types"],"ArrayBase") "allocUI16", b, reg_int ctx (List.length el)));
		| HF32 ->
			let b = alloc_tmp ctx HBytes in
			let size = reg_int ctx ((List.length el) * 4) in
			op ctx (OCall1 (b,alloc_std ctx "alloc_bytes" [HI32] HBytes,size));
			list_iteri (fun i e ->
				let r = eval_to ctx e HF32 in
				op ctx (OSetF32 (b,reg_int ctx (i * 4),r));
			) el;
			op ctx (OCall2 (r, alloc_fun_path ctx (["hl";"types"],"ArrayBase") "allocF32", b, reg_int ctx (List.length el)));
		| HF64 ->
			let b = alloc_tmp ctx HBytes in
			let size = reg_int ctx ((List.length el) * 8) in
			op ctx (OCall1 (b,alloc_std ctx "alloc_bytes" [HI32] HBytes,size));
			list_iteri (fun i e ->
				let r = eval_to ctx e HF64 in
				op ctx (OSetF64 (b,reg_int ctx (i * 8),r));
			) el;
			op ctx (OCall2 (r, alloc_fun_path ctx (["hl";"types"],"ArrayBase") "allocF64", b, reg_int ctx (List.length el)));
		| _ ->
			let at = if is_dynamic et then et else HDyn in
			let a = alloc_tmp ctx HArray in
			let rt = alloc_tmp ctx HType in
			op ctx (OType (rt,at));
			let size = reg_int ctx (List.length el) in
			op ctx (OCall2 (a,alloc_std ctx "alloc_array" [HType;HI32] HArray,rt,size));
			list_iteri (fun i e ->
				let r = eval_to ctx e at in
				op ctx (OSetArray (a,reg_int ctx i,r));
			) el;
			let tmp = if et = HDyn then alloc_tmp ctx (class_type ctx ctx.array_impl.aobj [] false) else r in
			op ctx (OCall1 (tmp, alloc_fun_path ctx (["hl";"types"],"ArrayObj") "alloc", a));
			if tmp <> r then begin
				let re = alloc_tmp ctx HBool in
				op ctx (OBool (re,true));
				let ren = alloc_tmp ctx (HNull HBool) in
				op ctx (OToDyn (ren, re));
				op ctx (OCall2 (r, alloc_fun_path ctx (["hl";"types"],"ArrayDyn") "alloc", tmp, ren));
			end;
		);
		r
	| TArray _ ->
		(match get_access ctx e with
		| AArray (a,at,idx) ->
			array_read ctx a at idx e.epos
		| _ ->
			assert false)
	| TMeta (_,e) ->
		eval_expr ctx e
	| TFor _ ->
		assert false (* eliminated by analyzer *)
	| TSwitch (en,cases,def) ->
		let rt = to_type ctx e.etype in
		let r = alloc_tmp ctx rt in
		(try
			let max = ref (-1) in
			let rec get_int e =
				match e.eexpr with
				| TConst (TInt i) ->
					let v = Int32.to_int i in
					if Int32.of_int v <> i then raise Exit;
					v
				| _ ->
					raise Exit
			in
			List.iter (fun (values,_) ->
				List.iter (fun v ->
					let i = get_int v in
					if i < 0 then raise Exit;
					if i > !max then max := i;
				) values;
			) cases;
			if !max > 255 || cases = [] then raise Exit;
			let ridx = eval_to ctx en HI32 in
			let indexes = Array.make (!max + 1) 0 in
			op ctx (OSwitch (ridx,indexes,0));
			let switch_pos = current_pos ctx in
			(match def with
			| None ->
				if rt <> HVoid then op ctx (ONull r);
			| Some e ->
				let re = eval_to ctx e rt in
				if rt <> HVoid then op ctx (OMov (r,re)));
			let jends = ref [jump ctx (fun i -> OJAlways i)] in
			List.iter (fun (values,ecase) ->
				List.iter (fun v ->
					Array.set indexes (get_int v) (current_pos ctx - switch_pos)
				) values;
				let re = eval_to ctx ecase rt in
				op ctx (OMov (r,re));
				jends := jump ctx (fun i -> OJAlways i) :: !jends
			) cases;
			DynArray.set ctx.m.mops (switch_pos - 1) (OSwitch (ridx,indexes,current_pos ctx - switch_pos));
			List.iter (fun j -> j()) (!jends);
		with Exit ->
			let jends = ref [] in
			let rvalue = eval_expr ctx en in
			let loop (cases,e) =
				let ok = List.map (fun c ->
					let r = eval_to ctx c (common_type ctx en c true c.epos) in
					jump ctx (fun n -> OJEq (r,rvalue,n))
				) cases in
				(fun() ->
					List.iter (fun f -> f()) ok;
					let re = eval_to ctx e rt in
					if rt <> HVoid then op ctx (OMov (r,re));
					jends := jump ctx (fun n -> OJAlways n) :: !jends)
			in
			let all = List.map loop cases in
			(match def with
			| None ->
				if rt <> HVoid then op ctx (ONull r)
			| Some e ->
				let rdef = eval_to ctx e rt in
				if rt <> HVoid then op ctx (OMov (r,rdef)));
			jends := jump ctx (fun n -> OJAlways n) :: !jends;
			List.iter (fun f -> f()) all;
			List.iter (fun j -> j()) (!jends);
		);
		r
	| TEnumParameter (ec,f,index) ->
		let r = alloc_tmp ctx (match to_type ctx ec.etype with HEnum e -> let _,_,args = e.efields.(f.ef_index) in args.(index) | _ -> assert false) in
		op ctx (OEnumField (r,eval_expr ctx ec,f.ef_index,index));
		cast_to ctx r (to_type ctx e.etype) e.epos
	| TContinue ->
		let pos = current_pos ctx in
		op ctx (OJAlways (-1)); (* loop *)
		ctx.m.mcontinues <- (fun target -> DynArray.set ctx.m.mops pos (OJAlways (target - (pos + 1)))) :: ctx.m.mcontinues;
		alloc_tmp ctx HVoid
	| TBreak ->
		let pos = current_pos ctx in
		op ctx (OJAlways (-1)); (* loop *)
		ctx.m.mbreaks <- (fun target -> DynArray.set ctx.m.mops pos (OJAlways (target - (pos + 1)))) :: ctx.m.mbreaks;
		alloc_tmp ctx HVoid
	| TTry (etry,catches) ->
		let pos = current_pos ctx in
		let rtrap = alloc_tmp ctx HDyn in
		op ctx (OTrap (rtrap,-1)); (* loop *)
		ctx.m.mtrys <- ctx.m.mtrys + 1;
		let tret = to_type ctx e.etype in
		let result = alloc_tmp ctx tret in
		let r = eval_expr ctx etry in
		if tret <> HVoid then op ctx (OMov (result,cast_to ctx r tret etry.epos));
		ctx.m.mtrys <- ctx.m.mtrys - 1;
		op ctx (OEndTrap true);
		let j = jump ctx (fun n -> OJAlways n) in
		DynArray.set ctx.m.mops pos (OTrap (rtrap, current_pos ctx - (pos + 1)));
		let rec loop l =
			match l with
			| [] ->
				op ctx (ORethrow rtrap);
				[]
			| (v,ec) :: next ->
				let rv = alloc_reg ctx v in
				let jnext = if v.v_type == t_dynamic then begin
					op ctx (OMov (rv, rtrap));
					(fun() -> ())
				end else
					let ct = (match follow v.v_type with
					| TInst (c,_) -> TClassDecl c
					| TAbstract (a,_) -> TAbstractDecl a
					| TEnum (e,_) -> TEnumDecl e
					| _ -> assert false
					) in
					let r = type_value ctx ct ec.epos in
					let rb = alloc_tmp ctx HBool in
					op ctx (OCall2 (rb, alloc_fun_path ctx (["hl";"types"],"BaseType") "check",r,rtrap));
					let jnext = jump ctx (fun n -> OJFalse (rb,n)) in
					op ctx (OMov (rv, unsafe_cast_to ctx rtrap (to_type ctx v.v_type) ec.epos));
					jnext
				in
				let r = eval_expr ctx ec in
				if tret <> HVoid then op ctx (OMov (result,cast_to ctx r tret ec.epos));
				if v.v_type == t_dynamic then [] else
				let jend = jump ctx (fun n -> OJAlways n) in
				jnext();
				jend :: loop next
		in
		List.iter (fun j -> j()) (loop catches);
		j();
		result
	| TTypeExpr t ->
		type_value ctx t e.epos
	| TCast (ev,Some _) ->
		let t = to_type ctx e.etype in
		let re = eval_expr ctx ev in
		let r = alloc_tmp ctx t in
		if safe_cast (rtype ctx re) t then
			op ctx (OMov (r,re))
		else
			op ctx (OSafeCast (r,re));
		r

and gen_assign_op ctx acc e1 f =
	let f r =
		match rtype ctx r with
		| HNull t ->
			let r2 = alloc_tmp ctx t in
			op ctx (OSafeCast (r2,r));
			let r3 = alloc_tmp ctx (HNull t) in
			op ctx (OToDyn (r3,f r2));
			r3
		| _ ->
			f r
	in
	match acc with
	| AInstanceField (eobj, findex) ->
		let robj = eval_null_check ctx eobj in
		let t = real_type ctx e1 in
		let r = alloc_tmp ctx t in
		op ctx (OField (r,robj,findex));
		let r = cast_to ctx r (to_type ctx e1.etype) e1.epos in
		let r = f r in
		op ctx (OSetField (robj,findex,cast_to ctx r t e1.epos));
		r
	| AStaticVar (g,t,fid) ->
		let o = alloc_tmp ctx t in
		op ctx (OGetGlobal (o,g));
		let r = alloc_tmp ctx (to_type ctx e1.etype) in
		op ctx (OField (r,o,fid));
		let r = f r in
		op ctx (OSetField (o,fid,r));
		r
	| AGlobal g ->
		let r = alloc_tmp ctx (to_type ctx e1.etype) in
		op ctx (OGetGlobal (r,g));
		let r = f r in
		op ctx (OSetGlobal (g,r));
		r
	| ACaptured idx ->
		let r = alloc_tmp ctx (to_type ctx e1.etype) in
		op ctx (OEnumField (r, ctx.m.mcaptreg, 0, idx));
		let r = f r in
		op ctx (OSetEnumField (ctx.m.mcaptreg,idx,r));
		r
	| AArray (ra,(at,_),ridx) ->
		(match at with
		| HDyn ->
			(* call getDyn() *)
			let r = alloc_tmp ctx HDyn in
			op ctx (OCallMethod (r,0,[ra;ridx]));
			let r = f r in
			(* call setDyn() *)
			op ctx (OCallMethod (alloc_tmp ctx HVoid,1,[ra;ridx;r]));
			r
		| _ ->
			(* bounds check against length *)
			let len = alloc_tmp ctx HI32 in
			op ctx (OField (len,ra,0)); (* length *)
			let j = jump ctx (fun i -> OJULt (ridx,len,i)) in
			op ctx (OCall2 (alloc_tmp ctx HVoid, alloc_fun_path ctx (array_class ctx at).cl_path "__expand", ra, ridx));
			j();
			match at with
			| HI32 | HF64 ->
				let hbytes = alloc_tmp ctx HBytes in
				op ctx (OField (hbytes, ra, 1));
				let ridx = shl ctx ridx (type_size_bits at) in
				let r = alloc_tmp ctx at in
				read_mem ctx r hbytes ridx at;
				let r = f r in
				write_mem ctx hbytes ridx at r;
				r
			| _ ->
				let arr = alloc_tmp ctx HArray in
				op ctx (OField (arr,ra,1));
				let r = alloc_tmp ctx at in
				op ctx (OGetArray (r,arr,ridx));
				let r = f r in
				op ctx (OSetArray (arr,ridx,r));
				r
		)
	| ADynamic (eobj, fid) ->
		let robj = eval_null_check ctx eobj in
		let t = real_type ctx e1 in
		let r = alloc_tmp ctx t in
		op ctx (ODynGet (r,robj,fid));
		let r = cast_to ctx r (to_type ctx e1.etype) e1.epos in
		let r = f r in
		op ctx (ODynSet (robj,fid,cast_to ctx r t e1.epos));
		r
	| ANone | ALocal _ | AStaticFun _ | AInstanceFun _ | AInstanceProto _ | AVirtualMethod _ | AEnum _ ->
		assert false

and build_capture_vars ctx f =
	let ignored_vars = ref PMap.empty in
	let used_vars = ref PMap.empty in
	(* get all captured vars in scope, ignore vars that are declared *)
	let decl_var v =
		if v.v_capture then ignored_vars := PMap.add v.v_id () !ignored_vars
	in
	let use_var v =
		if v.v_capture then used_vars := PMap.add v.v_id v !used_vars
	in
	let rec loop e =
		(match e.eexpr with
		| TLocal v ->
			use_var v;
		| TVar (v,_) ->
			decl_var v
		| TTry (_,catches) ->
			List.iter (fun (v,_) -> decl_var v) catches
		| TFunction f ->
			List.iter (fun (v,_) -> decl_var v) f.tf_args;
		| _ ->
			()
		);
		Type.iter loop e
	in
	List.iter (fun (v,_) -> decl_var v) f.tf_args;
	loop f.tf_expr;
	let cvars = Array.of_list (PMap.fold (fun v acc -> if PMap.mem v.v_id !ignored_vars then acc else v :: acc) !used_vars []) in
	Array.sort (fun v1 v2 -> v1.v_id - v2.v_id) cvars;
	let indexes = ref PMap.empty in
	Array.iteri (fun i v -> indexes := PMap.add v.v_id i !indexes) cvars;
	{
		c_map = !indexes;
		c_vars = cvars;
		c_type = HEnum {
			eglobal = None;
			ename = "";
			eid = 0;
			efields = [|"",0,Array.map (fun v -> to_type ctx v.v_type) cvars|];
		};
	}

and gen_method_wrapper ctx rt t p =
	try
		PMap.find (rt,t) ctx.method_wrappers
	with Not_found ->
		let fid = lookup_alloc ctx.cfids () in
		ctx.method_wrappers <- PMap.add (rt,t) fid ctx.method_wrappers;
		let old = ctx.m in
		let targs, tret = (match t with HFun (args, ret) -> args, ret | _ -> assert false) in
		let iargs, iret = (match rt with HFun (args, ret) -> args, ret | _ -> assert false) in
		ctx.m <- method_context fid HDyn null_capture;
		let rfun = alloc_tmp ctx rt in
		let rargs = List.map (alloc_tmp ctx) targs in
		let rret = alloc_tmp ctx iret in
		op ctx (OCallClosure (rret,rfun,List.map2 (fun r t -> cast_to ctx r t p) rargs iargs));
		op ctx (ORet (cast_to ctx rret tret p));
		let f = {
			name = "","";
			findex = fid;
			ftype = HFun (rt :: targs, tret);
			regs = DynArray.to_array ctx.m.mregs.arr;
			code = DynArray.to_array ctx.m.mops;
			debug = DynArray.to_array ctx.m.mdebug;
		} in
		ctx.m <- old;
		DynArray.add ctx.cfunctions f;
		fid

and make_fun ?gen_content ctx name fidx f cthis cparent =
	let old = ctx.m in
	let capt = build_capture_vars ctx f in
	let has_captured_vars = Array.length capt.c_vars > 0 in
	let capt, use_parent_capture = (match cparent with
		| Some cparent when has_captured_vars && List.for_all (fun v -> PMap.mem v.v_id cparent.c_map) (Array.to_list capt.c_vars) -> cparent, true
		| _ -> capt, false
	) in

	ctx.m <- method_context fidx (to_type ctx f.tf_type) capt;

	set_curpos ctx f.tf_expr.epos;

	let tthis = (match cthis with
	| None -> None
	| Some c ->
		let t = to_type ctx (TInst (c,[])) in
		ignore(alloc_tmp ctx t); (* index 0 *)
		Some t
	) in

	let rcapt = if has_captured_vars && cparent <> None then Some (alloc_tmp ctx capt.c_type) else None in

	let args = List.map (fun (v,o) ->
		let r = alloc_reg ctx (if o = None then v else { v with v_type = ctx.com.basic.tnull v.v_type }) in
		rtype ctx r
	) f.tf_args in

	if has_captured_vars then ctx.m.mcaptreg <- (match rcapt with
		| None ->
			let r = alloc_tmp ctx capt.c_type in
			op ctx (OEnumAlloc (r,0));
			r
		| Some r -> r
	);

	List.iter (fun (v, o) ->
		let r = alloc_reg ctx v in
		(match o with
		| None | Some TNull -> ()
		| Some c ->
			let j = jump ctx (fun n -> OJNotNull (r,n)) in
			(match c with
			| TNull | TThis | TSuper -> assert false
			| TInt i when (match to_type ctx (follow v.v_type) with HUI8 | HUI16 | HI32 | HDyn -> true | _ -> false) ->
				let tmp = alloc_tmp ctx HI32 in
				op ctx (OInt (tmp, alloc_i32 ctx i));
				op ctx (OToDyn (r, tmp));
			| TFloat s when (match to_type ctx (follow v.v_type) with HUI8 | HUI16 | HI32 -> true | _ -> false) ->
				let tmp = alloc_tmp ctx HI32 in
				op ctx (OInt (tmp, alloc_i32 ctx (Int32.of_float (float_of_string s))));
				op ctx (OToDyn (r, tmp));
			| TInt i ->
				let tmp = alloc_tmp ctx HF64 in
				op ctx (OFloat (tmp, alloc_float ctx (Int32.to_float i)));
				op ctx (OToDyn (r, tmp));
			| TFloat s ->
				let tmp = alloc_tmp ctx HF64 in
				op ctx (OFloat (tmp, alloc_float ctx (float_of_string s)));
				op ctx (OToDyn (r, tmp));
			| TBool b ->
				let tmp = alloc_tmp ctx HBool in
				op ctx (OBool (tmp, b));
				op ctx (OToDyn (r, tmp));
			| TString s ->
				let str, len = to_utf8 s f.tf_expr.epos in
				let rb = alloc_tmp ctx HBytes in
				op ctx (ONew r);
				op ctx (OString (rb,alloc_string ctx str));
				op ctx (OSetField (r,0,rb));
				op ctx (OSetField (r,1,reg_int ctx len));
			);
			j();
			(* if optional but not null, turn into a not nullable here *)
			let vt = to_type ctx v.v_type in
			if not (is_nullable vt) then begin
				let t = alloc_tmp ctx vt in
				ctx.m.mregs.map <- PMap.add v.v_id t ctx.m.mregs.map;
				op ctx (OSafeCast (t,r));
			end;
		);
		(match captured_index ctx v with
		| None -> ()
		| Some index ->
			op ctx (OSetEnumField (ctx.m.mcaptreg, index, alloc_reg ctx v)));
	) f.tf_args;

	(match gen_content with
	| None -> ()
	| Some f -> f());

	ignore(eval_expr ctx f.tf_expr);
	let tret = to_type ctx f.tf_type in
	let rec has_final_jump e =
		(* prevents a jump outside function bounds error *)
		match e.eexpr with
		| TBlock el -> (match List.rev el with e :: _ -> has_final_jump e | [] -> false)
		| TParenthesis e -> has_final_jump e
		| TReturn _ -> false
		| _ -> true
	in
	if tret = HVoid then
		op ctx (ORet (alloc_tmp ctx HVoid))
	else if has_final_jump f.tf_expr then begin
		let r = alloc_tmp ctx tret in
		(match tret with
		| HI32 | HUI8 | HUI16 -> op ctx (OInt (r,alloc_i32 ctx 0l))
		| HF32 | HF64 -> op ctx (OFloat (r,alloc_float ctx 0.))
		| HBool -> op ctx (OBool (r,false))
		| _ -> op ctx (ONull r));
		op ctx (ORet r)
	end;
	let fargs = (match tthis with None -> [] | Some t -> [t]) @ (match rcapt with None -> [] | Some r -> [rtype ctx r]) @ args in
	let f = {
		name = name;
		findex = fidx;
		ftype = HFun (fargs, tret);
		regs = DynArray.to_array ctx.m.mregs.arr;
		code = DynArray.to_array ctx.m.mops;
		debug = DynArray.to_array ctx.m.mdebug;
	} in
	ctx.m <- old;
	Hashtbl.add ctx.defined_funs fidx ();
	let f = if ctx.optimize then Hlopt.optimize ctx.dump_out f else f in
	DynArray.add ctx.cfunctions f;
	capt

let generate_static ctx c f =
	match f.cf_kind with
	| Var _ ->
		()
	| Method m ->
		let add_native lib name =
			ignore(lookup ctx.cnatives (name ^ "@" ^ lib) (fun() ->
				let fid = alloc_fid ctx c f in
				Hashtbl.add ctx.defined_funs fid ();
				(alloc_string ctx lib, alloc_string ctx name,to_type ctx f.cf_type,fid)
			));
		in
		let rec loop = function
			| (Meta.Custom ":hlNative",[(EConst(String(lib)),_);(EConst(String(name)),_)] ,_ ) :: _ ->
				add_native lib name
			| (Meta.Custom ":hlNative",[(EConst(String(lib)),_)] ,_ ) :: _ ->
				add_native lib f.cf_name
			| (Meta.Custom ":hlNative",[] ,_ ) :: _ ->
				add_native "std" f.cf_name
			| (Meta.Custom ":hlNative",_ ,p) :: _ ->
				abort "Invalid @:hlNative decl" p
			| [] ->
				ignore(make_fun ctx ((underscore_class_name c),f.cf_name) (alloc_fid ctx c f) (match f.cf_expr with Some { eexpr = TFunction f } -> f | _ -> assert false) None None)
			| _ :: l ->
				loop l
		in
		loop f.cf_meta


let rec generate_member ctx c f =
	match f.cf_kind with
	| Var _ -> ()
	| Method m ->
		let gen_content = if f.cf_name <> "new" then None else Some (fun() ->
			(*
				init dynamic functions
			*)
			List.iter (fun f ->
				match f.cf_kind with
				| Method MethDynamic ->
					let r = alloc_tmp ctx (to_type ctx f.cf_type) in
					let fid = (match class_type ctx c (List.map snd c.cl_params) false with
						| HObj o -> (try fst (get_index f.cf_name o) with Not_found -> assert false)
						| _ -> assert false
					) in
					op ctx (OGetThis (r,fid));
					op ctx (OJNotNull (r,2));
					op ctx (OInstanceClosure (r,alloc_fid ctx c f,0));
					op ctx (OSetThis (fid,r));
				| _ -> ()
			) c.cl_ordered_fields;
		) in
		ignore(make_fun ?gen_content ctx (underscore_class_name c,f.cf_name) (alloc_fid ctx c f) (match f.cf_expr with Some { eexpr = TFunction f } -> f | _ -> abort "Missing function body" f.cf_pos) (Some c) None);
		if f.cf_name = "toString" && not (List.memq f c.cl_overrides) && not (PMap.mem "__string" c.cl_fields) && is_to_string f.cf_type then begin
			let p = f.cf_pos in
			(* function __string() return this.toString().bytes *)
			let ethis = mk (TConst TThis) (TInst (c,List.map snd c.cl_params)) p in
			let tstr = mk (TCall (mk (TField (ethis,FInstance(c,List.map snd c.cl_params,f))) f.cf_type p,[])) ctx.com.basic.tstring p in
			let cstr, cf_bytes = (try (match ctx.com.basic.tstring with TInst(c,_) -> c, PMap.find "bytes" c.cl_fields | _ -> assert false) with Not_found -> assert false) in
			let estr = mk (TReturn (Some (mk (TField (tstr,FInstance (cstr,[],cf_bytes))) cf_bytes.cf_type p))) ctx.com.basic.tvoid p in
			ignore(make_fun ctx (underscore_class_name c,"__string") (alloc_fun_path ctx c.cl_path "__string") { tf_expr = estr; tf_args = []; tf_type = cf_bytes.cf_type; } (Some c) None)
		end

let generate_type ctx t =
	match t with
	| TClassDecl { cl_interface = true }->
		()
	| TClassDecl c when c.cl_extern ->
		List.iter (fun f ->
			List.iter (fun (name,args,pos) ->
				match name with
				| Meta.Custom ":hlNative" -> generate_static ctx c f
				| _ -> ()
			) f.cf_meta
		) c.cl_ordered_statics
	| TClassDecl c ->
		List.iter (generate_static ctx c) c.cl_ordered_statics;
		(match c.cl_constructor with
		| None -> ()
		| Some f -> generate_member ctx c f);
		List.iter (generate_member ctx c) c.cl_ordered_fields;
	| TEnumDecl _ | TTypeDecl _ | TAbstractDecl _ ->
		()

let generate_static_init ctx =
	let exprs = ref [] in
	let t_void = ctx.com.basic.tvoid in

	let gen_content() =

		op ctx (OCall0 (alloc_tmp ctx HVoid, alloc_fun_path ctx ([],"Type") "init"));

		(* init class values *)
		List.iter (fun t ->
			match t with
			| TClassDecl c when not c.cl_extern && not (is_array_class (s_type_path c.cl_path) && snd c.cl_path <> "ArrayDyn") && c != ctx.core_type && c != ctx.core_enum ->

				let path = if c == ctx.array_impl.abase then [],"Array" else if c == ctx.base_class then [],"Class" else c.cl_path in

				let g, ct = class_global ~resolve:false ctx c in

				let index name =
					match ct with
					| HObj o ->
						fst (try get_index name o with Not_found -> assert false)
					| _ ->
						assert false
				in

				let rc = alloc_tmp ctx ct in
				op ctx (ONew rc);
				op ctx (OSetGlobal (g,rc));

				let rt = alloc_tmp ctx HType in
				let ctype = if c == ctx.array_impl.abase then ctx.array_impl.aall else c in
				op ctx (OType (rt, class_type ctx ctype (List.map snd ctype.cl_params) false));
				op ctx (OSetField (rc,index "__type__",rt));
				op ctx (OSetField (rc,index "__name__",eval_expr ctx { eexpr = TConst (TString (s_type_path path)); epos = c.cl_pos; etype = ctx.com.basic.tstring }));

				let rname = alloc_tmp ctx HBytes in
				op ctx (OString (rname, alloc_string ctx (s_type_path path)));
				op ctx (OCall2 (alloc_tmp ctx HVoid, alloc_fun_path ctx ([],"Type") "register",rname,rc));

				(match c.cl_constructor with
				| None -> ()
				| Some f ->
					(* set __constructor__ *)
					let r = alloc_tmp ctx (match to_type ctx f.cf_type with
						| HFun (args,ret) -> HFun (class_type ctx c (List.map snd c.cl_params) false :: args, ret)
						| _ -> assert false
					) in
					op ctx (OStaticClosure (r, alloc_fid ctx c f));
					op ctx (OSetField (rc,index "__constructor__",r)));

				let gather_implements() =
					let classes = ref [] in
					let rec lookup cv =
						List.exists (fun (i,_) -> i == c || lookup i) cv.cl_implements
					in
					let check = function
						| TClassDecl c when c.cl_interface = false && not c.cl_extern -> if lookup c then classes := c :: !classes
						| _ -> ()
					in
					List.iter check ctx.com.types;
					!classes
				in
				if c.cl_interface then begin
					let l = gather_implements() in
					let ra = alloc_tmp ctx HArray in
					let rt = alloc_tmp ctx HType in
					op ctx (OType (rt, HType));
					op ctx (OCall2 (ra, alloc_std ctx "alloc_array" [HType;HI32] HArray, rt, reg_int ctx (List.length l)));
					list_iteri (fun i intf ->
						op ctx (OType (rt, to_type ctx (TInst (intf,[]))));
						op ctx (OSetArray (ra, reg_int ctx i, rt));
					) l;
					op ctx (OSetField (rc,index "__implementedBy__",ra));
				end;

				(* register static funs *)

				List.iter (fun f ->
					match f.cf_kind with
					| Method _ when not (is_extern_field f) ->
						let cl = alloc_tmp ctx (to_type ctx f.cf_type) in
						op ctx (OStaticClosure (cl, alloc_fid ctx c f));
						op ctx (OSetField (rc,index f.cf_name,cl));
					| _ ->
						()
				) c.cl_ordered_statics;

				(match Codegen.build_metadata ctx.com (TClassDecl c) with
				| None -> ()
				| Some e ->
					let r = eval_to ctx e HDyn in
					op ctx (OSetField (rc,index "__meta__",r)));

			| TEnumDecl e when not e.e_extern ->

				let t = enum_class ctx e in
				let g = alloc_global ctx (match t with HObj o -> o.pname | _ -> assert false) t in

				let index name =
					match t with
					| HObj o ->
						fst (try get_index name o with Not_found -> assert false)
					| _ ->
						assert false
				in
				let r = alloc_tmp ctx t in
				let rt = alloc_tmp ctx HType in
				op ctx (ONew r);

				let max_val = ref (-1) in
				PMap.iter (fun _ c ->
					match follow c.ef_type with
					| TFun _ -> ()
					| _ -> if c.ef_index > !max_val then max_val := c.ef_index;
				) e.e_constrs;

				let avalues = alloc_tmp ctx HArray in
				op ctx (OType (rt, HDyn));
				op ctx (OCall2 (avalues, alloc_std ctx "alloc_array" [HType;HI32] HArray, rt, reg_int ctx (!max_val + 1)));

				List.iter (fun n ->
					let f = PMap.find n e.e_constrs in
					match follow f.ef_type with
					| TFun _ -> ()
					| _ ->
						let t = to_type ctx f.ef_type in
						let g = alloc_global ctx (efield_name e f) t in
						let r = alloc_tmp ctx t in
						op ctx (OMakeEnum (r,f.ef_index,[]));
						op ctx (OSetGlobal (g,r));
						let d = alloc_tmp ctx HDyn in
						op ctx (OToDyn (d,r));
						op ctx (OSetArray (avalues, reg_int ctx f.ef_index, d));
				) e.e_names;

				op ctx (OType (rt, (to_type ctx (TEnum (e,List.map snd e.e_params)))));
				op ctx (OCall3 (alloc_tmp ctx HVoid, alloc_fun_path ctx (["hl";"types"],"Enum") "new",r,rt,avalues));

				(match Codegen.build_metadata ctx.com (TEnumDecl e) with
				| None -> ()
				| Some e -> op ctx (OSetField (r,index "__meta__",eval_to ctx e HDyn)));

				op ctx (OSetGlobal (g,r));

			| TAbstractDecl { a_path = [], name; a_pos = pos } ->
				(match name with
				| "Int" | "Float" | "Dynamic" | "Bool" ->
					let is_bool = name = "Bool" in
					let t = class_type ctx (if is_bool then ctx.core_enum else ctx.core_type) [] false in

					let index name =
						match t with
						| HObj o ->
							fst (try get_index name o with Not_found -> assert false)
						| _ ->
							assert false
					in

					let g = alloc_global ctx ("$" ^ name) t in
					let r = alloc_tmp ctx t in
					let rt = alloc_tmp ctx HType in
					op ctx (ONew r);
					op ctx (OType (rt,(match name with "Int" -> HI32 | "Float" -> HF64 | "Dynamic" -> HDyn | "Bool" -> HBool | _ -> assert false)));
					op ctx (OSetField (r,index "__type__",rt));
					op ctx (OSetField (r,index (if is_bool then "__ename__" else "__name__"),make_string ctx name pos));
					op ctx (OSetGlobal (g,r));

					let bytes = alloc_tmp ctx HBytes in
					op ctx (OString (bytes, alloc_string ctx name));
					op ctx (OCall2 (alloc_tmp ctx HVoid, alloc_fun_path ctx ([],"Type") "register",bytes,r));
				| _ ->
					())
			| _ ->
				()

		) ctx.com.types;
	in
	(* init class statics *)
	List.iter (fun t ->
		match t with
		| TClassDecl c when not c.cl_extern ->
			(match c.cl_init with None -> () | Some e -> exprs := e :: !exprs);
			List.iter (fun f ->
				match f.cf_kind, f.cf_expr with
				| Var _, Some e ->
					let p = e.epos in
					let e = mk (TBinop (OpAssign,(mk (TField (mk (TTypeExpr t) t_dynamic p,FStatic (c,f))) f.cf_type p), e)) f.cf_type p in
					exprs := e :: !exprs;
				| _ ->
					()
			) c.cl_ordered_statics;
		| _ -> ()
	) ctx.com.types;
	(* call main() *)
	(match ctx.com.main with
	| None -> ()
	| Some e -> exprs := e :: !exprs);
	let fid = alloc_function_name ctx "<entry>" in
	ignore(make_fun ~gen_content ctx ("","") fid { tf_expr = mk (TBlock (List.rev !exprs)) t_void null_pos; tf_args = []; tf_type = t_void } None None);
	fid

(* --------------------------------------------------------------------------------------------------------------------- *)
(* WRITE *)


(* 	from -500M to +500M
	0[7] = 0-127
	10[+/-][5] [8] = -x2000/+x2000
	11[+/-][5] [24] = -x20000000/+x20000000
*)
let write_index_gen b i =
	if i < 0 then
		let i = -i in
		if i < 0x2000 then begin
			b ((i lsr 8) lor 0xA0);
			b (i land 0xFF);
		end else if i >= 0x20000000 then assert false else begin
			b ((i lsr 24) lor 0xE0);
			b ((i lsr 16) land 0xFF);
			b ((i lsr 8) land 0xFF);
			b (i land 0xFF);
		end
	else if i < 0x80 then
		b i
	else if i < 0x2000 then begin
		b ((i lsr 8) lor 0x80);
		b (i land 0xFF);
	end else if i >= 0x20000000 then assert false else begin
		b ((i lsr 24) lor 0xC0);
		b ((i lsr 16) land 0xFF);
		b ((i lsr 8) land 0xFF);
		b (i land 0xFF);
	end

let write_code ch code debug =

	let all_types, htypes = gather_types code in
	let byte = IO.write_byte ch in
	let write_index = write_index_gen byte in

	let rec write_type t =
		write_index (try PMap.find t htypes with Not_found -> assert false)
	in

	let write_op op =

		let o = Obj.repr op in
		let oid = Obj.tag o in

		match op with
		| OLabel _ ->
			byte oid
		| OCall2 (r,g,a,b) ->
			byte oid;
			write_index r;
			write_index g;
			write_index a;
			write_index b;
		| OCall3 (r,g,a,b,c) ->
			byte oid;
			write_index r;
			write_index g;
			write_index a;
			write_index b;
			write_index c;
		| OCall4 (r,g,a,b,c,d) ->
			byte oid;
			write_index r;
			write_index g;
			write_index a;
			write_index b;
			write_index c;
			write_index d;
		| OCallN (r,f,rl) | OCallClosure (r,f,rl) | OCallMethod (r,f,rl) | OCallThis (r,f,rl) | OMakeEnum (r,f,rl) ->
			byte oid;
			write_index r;
			write_index f;
			let n = List.length rl in
			if n > 0xFF then assert false;
			byte n;
			List.iter write_index rl
		| OType (r,t) ->
			byte oid;
			write_index r;
			write_type t
		| OSwitch (r,pl,eend) ->
			byte oid;
			write_index r;
			write_index (Array.length pl);
			Array.iter write_index pl;
			write_index eend
		| OEnumField (r,e,i,idx) ->
			byte oid;
			write_index r;
			write_index e;
			write_index i;
			write_index idx;
		| _ ->
			let field n = (Obj.magic (Obj.field o n) : int) in
			match Obj.size o with
			| 1 ->
				let a = field 0 in
				byte oid;
				write_index a;
			| 2 ->
				let a = field 0 in
				let b = field 1 in
				byte oid;
				write_index a;
				write_index b;
			| 3 ->
				let a = field 0 in
				let b = field 1 in
				let c = field 2 in
				byte oid;
				write_index a;
				write_index b;
				write_index c;
			| _ ->
				assert false
	in

	IO.nwrite ch "HLB";
	byte code.version;

	let flags = ref 0 in
	if debug then flags := !flags lor 1;
	byte !flags;

	write_index (Array.length code.ints);
	write_index (Array.length code.floats);
	write_index (Array.length code.strings);
	write_index (Array.length all_types);
	write_index (Array.length code.globals);
	write_index (Array.length code.natives);
	write_index (Array.length code.functions);
	write_index code.entrypoint;

	Array.iter (IO.write_real_i32 ch) code.ints;
	Array.iter (IO.write_double ch) code.floats;

	let write_strings strings =
		let str_length = ref 0 in
		Array.iter (fun str -> str_length := !str_length + String.length str + 1) strings;
		IO.write_i32 ch !str_length;
		Array.iter (IO.write_string ch) strings;
		Array.iter (fun str -> write_index (String.length str)) strings;
	in
	write_strings code.strings;

	if debug then begin
		write_index (Array.length code.debugfiles);
		write_strings code.debugfiles;
	end;

	Array.iter (fun t ->
		match t with
		| HVoid -> byte 0
		| HUI8 -> byte 1
		| HUI16 -> byte 2
		| HI32 -> byte 3
		| HF32 -> byte 4
		| HF64 -> byte 5
		| HBool -> byte 6
		| HBytes -> byte 7
		| HDyn -> byte 8
		| HFun (args,ret) ->
			let n = List.length args in
			if n > 0xFF then assert false;
			byte 9;
			byte n;
			List.iter write_type args;
			write_type ret
		| HObj p ->
			byte 10;
			write_index p.pid;
			(match p.psuper with
			| None -> write_index (-1)
			| Some t -> write_type (HObj t));
			(match p.pclassglobal with
			| None -> write_index 0
			| Some g -> write_index (g + 1));
			write_index (Array.length p.pfields);
			write_index (Array.length p.pproto);
			Array.iter (fun (_,n,t) -> write_index n; write_type t) p.pfields;
			Array.iter (fun f -> write_index f.fid; write_index f.fmethod; write_index (match f.fvirtual with None -> -1 | Some i -> i)) p.pproto;
		| HArray ->
			byte 11
		| HType ->
			byte 12
		| HRef t ->
			byte 13;
			write_type t
		| HVirtual v ->
			byte 14;
			write_index (Array.length v.vfields);
			Array.iter (fun (_,sid,t) -> write_index sid; write_type t) v.vfields
		| HDynObj ->
			byte 15
		| HAbstract (_,i) ->
			byte 16;
			write_index i
		| HEnum e ->
			byte 17;
			write_index e.eid;
			(match e.eglobal with
			| None -> write_index 0
			| Some g -> write_index (g + 1));
			write_index (Array.length e.efields);
			Array.iter (fun (_,nid,tl) ->
				write_index nid;
				if Array.length tl > 0xFF then assert false;
				byte (Array.length tl);
				Array.iter write_type tl;
			) e.efields
		| HNull t ->
			byte 18;
			write_type t
	) all_types;

	let write_debug_infos debug =
		let curfile = ref (-1) in
		let curpos = ref 0 in
		let rcount = ref 0 in
		let rec flush_repeat p =
			if !rcount > 0 then begin
				if !rcount > 15 then begin
					byte ((15 lsl 2) lor 2);
					rcount := !rcount - 15;
					flush_repeat(p)
				end else begin
					let delta = p - !curpos in
					let delta = (if delta > 0 && delta < 4 then delta else 0) in
					byte ((delta lsl 6) lor (!rcount lsl 2) lor 2);
					rcount := 0;
					curpos := !curpos + delta;
				end
			end
		in
		Array.iter (fun (f,p) ->
			if f <> !curfile then begin
				flush_repeat(p);
				curfile := f;
				byte ((f lsr 7) lor 1);
				byte (f land 0xFF);
			end;
			if p <> !curpos then flush_repeat(p);
			if p = !curpos then
				rcount := !rcount + 1
			else
				let delta = p - !curpos in
				if delta > 0 && delta < 32 then
					byte ((delta lsl 3) lor 4)
				else begin
					byte (p lsl 3);
					byte (p lsr 5);
					byte (p lsr 13);
				end;
				curpos := p;
		) debug;
		flush_repeat(!curpos)
	in

	Array.iter write_type code.globals;
	Array.iter (fun (lib_index, name_index,ttype,findex) ->
		write_index lib_index;
		write_index name_index;
		write_type ttype;
		write_index findex;
	) code.natives;
	Array.iter (fun f ->
		write_type f.ftype;
		write_index f.findex;
		write_index (Array.length f.regs);
		write_index (Array.length f.code);
		Array.iter write_type f.regs;
		Array.iter write_op f.code;
		if debug then write_debug_infos f.debug;
	) code.functions

(* --------------------------------------------------------------------------------------------------------------------- *)

let generate com =
	let get_class name =
		try
			match List.find (fun t -> (t_infos t).mt_path = (["hl";"types"],name)) com.types with
			| TClassDecl c -> c
			| _ -> assert false
		with
			Not_found ->
				failwith ("hl class " ^ name ^ " not found")
	in
	let dump = Common.defined com Define.Dump in
	let ctx = {
		com = com;
		optimize = not (Common.raw_defined com "hl-no-opt");
		dump_out = if dump then Some (IO.output_channel (open_out_bin "dump/hlopt.txt")) else None;
		m = method_context 0 HVoid null_capture;
		cints = new_lookup();
		cstrings = new_lookup();
		cfloats = new_lookup();
		cglobals = new_lookup();
		cnatives = new_lookup();
		cfunctions = DynArray.create();
		overrides = Hashtbl.create 0;
		cached_types = PMap.empty;
		cfids = new_lookup();
		defined_funs = Hashtbl.create 0;
		array_impl = {
			aall = get_class "ArrayAccess";
			abase = get_class "ArrayBase";
			adyn = get_class "ArrayDyn";
			aobj = get_class "ArrayObj";
			aui16 = get_class "ArrayBasic_hl_types_UI16";
			ai32 = get_class "ArrayBasic_Int";
			af32 = get_class "ArrayBasic_Single";
			af64 = get_class "ArrayBasic_Float";
		};
		base_class = get_class "Class";
		base_enum = get_class "Enum";
		base_type = get_class "BaseType";
		core_type = get_class "CoreType";
		core_enum = get_class "CoreEnum";
		anons_cache = [];
		rec_cache = [];
		method_wrappers = PMap.empty;
		cdebug_files = new_lookup();
	} in
	let all_classes = Hashtbl.create 0 in
	List.iter (fun t ->
		match t with
		| TClassDecl ({ cl_path = ["hl";"types"], ("BasicIterator"|"ArrayBasic") } as c) ->
			c.cl_extern <- true
		| TClassDecl c ->
			let rec loop p f =
				match p with
				| Some (p,_) when PMap.mem f.cf_name p.cl_fields || loop p.cl_super f ->
					Hashtbl.replace ctx.overrides (f.cf_name,p.cl_path) true;
					true
				| _ ->
					false
			in
			List.iter (fun f -> ignore(loop c.cl_super f)) c.cl_overrides;
			Hashtbl.add all_classes c.cl_path c;
			List.iter (fun (m,args,p) ->
				if m = Meta.Custom ":hlNative" then
					let lib, prefix = (match args with
					| [(EConst (String lib),_)] -> lib, ""
					| [(EConst (String lib),_);(EConst (String p),_)] -> lib, p
					| _ -> abort "hlNative on class requires library name" p
					) in
					(* adds :hlNative for all empty methods *)
					List.iter (fun f ->
						match f.cf_kind with
						| Method MethNormal when not (List.exists (fun (m,_,_) -> m = Meta.Custom ":hlNative") f.cf_meta) ->
							(match f.cf_expr with
							| Some { eexpr = TFunction { tf_expr = { eexpr = TBlock ([] | [{ eexpr = TReturn (Some { eexpr = TConst _ })}]) } } } ->
								let name = prefix ^ String.lowercase (Str.global_replace (Str.regexp "[A-Z]+") "_\\0" f.cf_name) in
								f.cf_meta <- (Meta.Custom ":hlNative", [(EConst (String lib),p);(EConst (String name),p)], p) :: f.cf_meta;
							| _ -> ())
						| _ -> ()
					) c.cl_ordered_statics
			) c.cl_meta;
 		| _ -> ()
	) com.types;
	ignore(alloc_string ctx "");
	ignore(class_type ctx ctx.base_class [] false);
	List.iter (generate_type ctx) com.types;
	let ep = generate_static_init ctx in
	let code = {
		version = 1;
		entrypoint = ep;
		strings = DynArray.to_array ctx.cstrings.arr;
		ints = DynArray.to_array ctx.cints.arr;
		floats = DynArray.to_array ctx.cfloats.arr;
		globals = DynArray.to_array ctx.cglobals.arr;
		natives = DynArray.to_array ctx.cnatives.arr;
		functions = DynArray.to_array ctx.cfunctions;
		debugfiles = DynArray.to_array ctx.cdebug_files.arr;
	} in
	Array.sort (fun (lib1,_,_,_) (lib2,_,_,_) -> lib1 - lib2) code.natives;
	if dump then begin
		(match ctx.dump_out with None -> () | Some ch -> IO.close_out ch);
		let ch = open_out_bin "dump/hlcode.txt" in
		Hlcode.dump (fun s -> output_string ch (s ^ "\n")) code;
		close_out ch;
	end;
	if Common.raw_defined com "hl-check" then begin
		PMap.iter (fun (s,p) fid ->
			if not (Hashtbl.mem ctx.defined_funs fid) then failwith (Printf.sprintf "Unresolved method %s:%s(@%d)" (s_type_path p) s fid)
		) ctx.cfids.map;
		Hlinterp.check code;
	end;
	let t = Common.timer "write hl" in
	if file_extension com.file = "c" then
		Hl2c.write_c com.Common.version com.file code
	else begin
		let ch = IO.output_string() in
		write_code ch code true;
		let str = IO.close_out ch in
		let ch = open_out_bin com.file in
		output_string ch str;
		close_out ch;
	end;
	t();
	if Common.defined com Define.Interp then try ignore(Hlinterp.interp code) with Failure msg -> abort msg Ast.null_pos
