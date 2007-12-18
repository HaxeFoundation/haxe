open As3
open As3hl

type parse_ctx = {
	as3 : as3_tag;
	mutable namespaces : hl_namespace array;
	mutable nsets : hl_ns_set array;
	mutable names : hl_name array;
	mutable methods : hl_method array;
	mutable classes : hl_class array;
	mutable jumps : (int * int) list;
	mutable pos : int;
}

let get = As3parse.iget
let no_nz = As3parse.no_nz
let idx n = As3parse.index_int n - 1

let ident ctx i = get ctx.as3.as3_idents i
let name ctx n = ctx.names.(idx n)
let method_type ctx n = ctx.methods.(idx (no_nz n))
let getclass ctx n = ctx.classes.(idx (no_nz n))

let global_mark = ref 0

let opt f ctx = function
	| None -> None
	| Some x -> Some (f ctx x)

let parse_opcode ctx i = function
	| A3BreakPoint -> HBreakPoint
	| A3Nop -> HNop
	| A3Throw -> HThrow
	| A3GetSuper n -> HGetSuper (name ctx n)
	| A3SetSuper n -> HSetSuper (name ctx n)
	| A3RegKill r -> HRegKill r
	| A3Label -> HLabel
	| A3Jump (j,n) -> 
		ctx.jumps <- (i,ctx.pos) :: ctx.jumps;
		HJump (j,n)
	| A3Switch (n,infos) ->
		ctx.jumps <- (i,ctx.pos) :: ctx.jumps;
		HSwitch(n,infos)
	| A3PushWith -> HPushWith
	| A3PopScope -> HPopScope
	| A3ForIn -> HForIn
	| A3HasNext -> HHasNext
	| A3Null -> HNull
	| A3Undefined -> HUndefined
	| A3ForEach -> HForEach
	| A3SmallInt n -> HSmallInt n
	| A3Int n -> HInt n
	| A3True -> HTrue
	| A3False -> HFalse
	| A3NaN -> HNaN
	| A3Pop -> HPop
	| A3Dup -> HDup
	| A3Swap -> HSwap
	| A3String i -> HString (ident ctx i)
	| A3IntRef i -> HIntRef (get ctx.as3.as3_ints i)
	| A3UIntRef i -> HUIntRef (get ctx.as3.as3_uints i)
	| A3Float f -> HFloat (get ctx.as3.as3_floats f)
	| A3Scope -> HScope
	| A3Namespace n -> HNamespace ctx.namespaces.(idx n)
	| A3Next (r1,r2) -> HNext (r1,r2)
	| A3Function f -> HFunction (method_type ctx f)
	| A3CallStack n -> HCallStack n
	| A3Construct n -> HConstruct n
	| A3CallMethod (s,n) -> HCallMethod (s,n)
	| A3CallStatic (m,n) -> HCallStatic (ctx.methods.(idx m),n)
	| A3CallSuper (p,n) -> HCallSuper (name ctx p,n)
	| A3CallProperty (p,n) -> HCallProperty (name ctx p,n)
	| A3RetVoid -> HRetVoid
	| A3Ret -> HRet
	| A3ConstructSuper n -> HConstructSuper n
	| A3ConstructProperty (p,n) -> HConstructProperty (name ctx p,n)
	| A3CallPropLex (p,n) -> HCallPropLex (name ctx p,n)
	| A3CallSuperVoid (p,n) -> HCallSuperVoid (name ctx p,n)
	| A3CallPropVoid (p,n) -> HCallPropVoid (name ctx p,n)
	| A3Object n -> HObject n
	| A3Array n -> HArray n
	| A3NewBlock -> HNewBlock
	| A3ClassDef n -> HClassDef (getclass ctx n)
	| A3Catch n -> HCatch n
	| A3FindPropStrict p -> HFindPropStrict (name ctx p)
	| A3FindProp p -> HFindProp (name ctx p)
	| A3FindDefinition p -> HFindDefinition (name ctx p)
	| A3GetLex p -> HGetLex (name ctx p)
	| A3SetProp p -> HSetProp (name ctx p)
	| A3Reg r -> HReg r
	| A3SetReg r -> HSetReg r
	| A3GetGlobalScope -> HGetGlobalScope
	| A3GetScope n -> HGetScope n
	| A3GetProp p -> HGetProp (name ctx p)
	| A3InitProp p -> HInitProp (name ctx p)
	| A3DeleteProp p -> HDeleteProp (name ctx p)
	| A3GetSlot n -> HGetSlot n
	| A3SetSlot n -> HSetSlot n
	| A3ToString -> HToString
	| A3ToXml -> HToXml
	| A3ToXmlAttr -> HToXmlAttr
	| A3ToInt -> HToInt
	| A3ToUInt -> HToUInt
	| A3ToNumber -> HToNumber
	| A3ToBool -> HToBool
	| A3ToObject -> HToObject
	| A3CheckIsXml -> HCheckIsXml
	| A3Cast p -> HCast (name ctx p)
	| A3AsAny -> HAsAny
	| A3AsString -> HAsString
	| A3AsType p -> HAsType (name ctx p)
	| A3AsObject -> HAsObject
	| A3IncrReg r -> HIncrReg r
	| A3DecrReg r -> HDecrReg r
	| A3Typeof -> HTypeof
	| A3InstanceOf -> HInstanceOf
	| A3IsType p -> HIsType (name ctx p)
	| A3IncrIReg r -> HIncrIReg r
	| A3DecrIReg r -> HDecrIReg r
	| A3This -> HThis
	| A3SetThis -> HSetThis
	| A3DebugReg (id,r,n) -> HDebugReg (ident ctx id,r,n)
	| A3DebugLine n -> HDebugLine n
	| A3DebugFile p -> HDebugFile (ident ctx p)
	| A3BreakPointLine n -> HBreakPointLine n
	| A3Timestamp -> HTimestamp
	| A3Op op -> HOp op
	| A3Unk n -> HUnk n

let parse_code ctx code =
	let old = ctx.pos , ctx.jumps in
	let indexes = DynArray.create() in
	ctx.pos <- 0;
	ctx.jumps <- [];
	let hcode = Array.mapi (fun i op ->
		let len = As3code.length op in
		DynArray.add indexes i;
		for k = 2 to len do DynArray.add indexes (-1); done;
		ctx.pos <- ctx.pos + len;
		parse_opcode ctx i op
	) code in
	(* in case we have a dead-jump at the end of code *)
	DynArray.add indexes (Array.length code);
	(* patch jumps *)
	List.iter (fun (j,pos) ->
		Array.set hcode j (match Array.get hcode j with
			| HJump (jc,n) -> 
				let idx = DynArray.get indexes (pos + n) in
				if idx = -1 then assert false;
				HJump (jc,idx - j)
			| HSwitch (n,infos) -> assert false
			| _ -> assert false)
	) ctx.jumps;
	ctx.pos <- fst old;
	ctx.jumps <- snd old;
	hcode

let parse_metadata ctx m =
	{
		hlmeta_name = ident ctx m.meta3_name;
		hlmeta_data = Array.map (fun (i1,i2) -> opt ident ctx i1, ident ctx i2) m.meta3_data;
	}

let parse_method ctx m =
	{
		hlm_type = method_type ctx m.m3_type;
		hlm_final = m.m3_final;
		hlm_override = m.m3_override;
		hlm_kind = m.m3_kind;
	}

let parse_value ctx = function
	| A3VNone -> HVNone
	| A3VNull -> HVNull
	| A3VBool b -> HVBool b
	| A3VString s -> HVString (ident ctx s)
	| A3VInt i -> HVInt (get ctx.as3.as3_ints i)
	| A3VUInt i -> HVUInt (get ctx.as3.as3_uints i)
	| A3VFloat f -> HVFloat (get ctx.as3.as3_floats f)
	| A3VNamespace (n,ns) -> HVNamespace (n,ctx.namespaces.(idx ns))

let parse_var ctx v =
	{
		hlv_type = opt name ctx v.v3_type;
		hlv_value = parse_value ctx v.v3_value;
		hlv_const = v.v3_const;
	}

let parse_field_kind ctx = function
	| A3FMethod m -> HFMethod (parse_method ctx m)
	| A3FVar v -> HFVar (parse_var ctx v)
	| A3FFunction f -> HFFunction (method_type ctx f)
	| A3FClass c -> HFClass (getclass ctx c)

let parse_field ctx f =
	{
		hlf_name = name ctx f.f3_name;
		hlf_slot = f.f3_slot;
		hlf_kind = parse_field_kind ctx f.f3_kind;
		hlf_metas =
			match f.f3_metas with
			| None -> None
			| Some a ->
				Some (Array.map (fun i ->
					parse_metadata ctx (get ctx.as3.as3_metadatas (no_nz i))
				) a);
	}

let parse_static ctx s =
	{
		hls_method = method_type ctx s.st3_method;
		hls_fields = Array.map (parse_field ctx) s.st3_fields;
	}

let parse_namespace ctx = function
	| A3NPrivate id -> HNPrivate (opt ident ctx id)
	| A3NPublic id -> HNPublic (opt ident ctx id)
	| A3NInternal id -> HNInternal (opt ident ctx id)
	| A3NProtected id -> HNProtected (ident ctx id)
	| A3NNamespace id -> HNNamespace (ident ctx id)
	| A3NExplicit id -> HNExplicit (ident ctx id)
	| A3NStaticProtected id -> HNStaticProtected (opt ident ctx id)

let parse_nset ctx l = List.map (fun n -> ctx.namespaces.(idx n)) l

let rec parse_name ctx = function
	| A3MName (id,ns) -> HMName (ident ctx id,ctx.namespaces.(idx ns))
	| A3MMultiName (id,ns) -> HMMultiName (opt ident ctx id,ctx.nsets.(idx ns))
	| A3MRuntimeName id -> HMRuntimeName (ident ctx id)
	| A3MRuntimeNameLate -> HMRuntimeNameLate
	| A3MMultiNameLate ns -> HMMultiNameLate ctx.nsets.(idx ns)
	| A3MAttrib multi -> HMAttrib (parse_name ctx multi)

let parse_try_catch ctx t =
	{
		hltc_start = t.tc3_start;
		hltc_end = t.tc3_end;
		hltc_handle = t.tc3_handle;
		hltc_type = opt name ctx t.tc3_type;
		hltc_name = opt name ctx t.tc3_name;
	}

let parse_function ctx m f =
	{
		hlf_stack_size = f.fun3_stack_size;
		hlf_nregs = f.fun3_nregs;
		hlf_init_scope = f.fun3_init_scope;
		hlf_max_scope = f.fun3_max_scope;
		hlf_code = [||]; (* keep for later *)
		hlf_trys = Array.map (parse_try_catch ctx) f.fun3_trys;
		hlf_locals = Array.map (fun f ->
			if f.f3_metas <> None then assert false;
			match f.f3_kind with
			| A3FVar v ->
				if v.v3_const || v.v3_value <> A3VNone then assert false;
				name ctx f.f3_name , opt name ctx v.v3_type , f.f3_slot
			| _ -> assert false
		) f.fun3_locals;
	}

let parse_method_type ctx m f =
	incr global_mark;
	{
		hlmt_ret = opt name ctx m.mt3_ret;
		hlmt_args = List.map (opt name ctx) m.mt3_args;
		hlmt_native = m.mt3_native;
		hlmt_var_args = m.mt3_var_args;
		hlmt_arguments_defined = m.mt3_arguments_defined;
		hlmt_uses_dxns = m.mt3_uses_dxns;
		hlmt_new_block = m.mt3_new_block;
		hlmt_unused_flag = m.mt3_unused_flag;
		hlmt_debug_name = opt ident ctx m.mt3_debug_name;
		hlmt_dparams = opt (fun ctx -> List.map (parse_value ctx)) ctx m.mt3_dparams;
		hlmt_pnames = opt (fun ctx -> List.map (ident ctx)) ctx m.mt3_pnames;
		hlmt_function = parse_function ctx m f;
		hlmt_mark = !global_mark;
	}

let parse_class ctx c s =
	{
		hlc_name = name ctx c.cl3_name;
		hlc_super = opt name ctx c.cl3_super;
		hlc_sealed = c.cl3_sealed;
		hlc_final = c.cl3_final;
		hlc_interface = c.cl3_interface;
		hlc_namespace = opt (fun ctx i -> ctx.namespaces.(idx i)) ctx c.cl3_namespace;
		hlc_implements = Array.map (name ctx) c.cl3_implements;
		hlc_construct = method_type ctx c.cl3_construct;
		hlc_fields = Array.map (parse_field ctx) c.cl3_fields;
		hlc_static_construct = method_type ctx s.st3_method;
		hlc_static_fields = Array.map (parse_field ctx) s.st3_fields;
	}

let parse_static ctx s =
	{
		hls_method = method_type ctx s.st3_method;
		hls_fields = Array.map (parse_field ctx) s.st3_fields;
	}

let parse t =
	let ctx = {
		as3 = t;
		namespaces = [||];
		nsets = [||];
		names = [||];
		methods = [||];
		classes = [||];
		jumps = [];
		pos = 0;
	} in
	ctx.namespaces <- Array.map (parse_namespace ctx) t.as3_namespaces;
	ctx.nsets <- Array.map (parse_nset ctx) t.as3_nsets;
	ctx.names <- Array.map (parse_name ctx) t.as3_names;
	ctx.methods <- Array.mapi (fun i f ->
		(* functions are supposed to be ordonned the same as method_types *)
		let idx = idx (no_nz f.fun3_id) in
		if idx <> i then assert false;
		parse_method_type ctx t.as3_method_types.(idx) f
	) t.as3_functions;
	ctx.classes <- Array.mapi (fun i c ->
		parse_class ctx c t.as3_statics.(i)
	) t.as3_classes;
	let inits = List.map (parse_static ctx) (Array.to_list t.as3_inits) in
	Array.iter (fun f ->
		let m = method_type ctx f.fun3_id in
		m.hlmt_function.hlf_code <- parse_code ctx f.fun3_code;
	) t.as3_functions;
	inits

(* ************************************************************************ *)
(*			FLATTEN															*)
(* ************************************************************************ *)

type ('hl,'item,'key) gen_lookup = {
	h : ('key,int) Hashtbl.t;
	a : 'item DynArray.t;
	f : flatten_ctx -> 'hl -> 'item;
	k : 'hl -> 'key;
}

and ('hl,'item) lookup = ('hl,'item,'hl) gen_lookup

and flatten_ctx = {
	fints : (hl_int,as3_int) lookup;
	fuints : (hl_uint,as3_uint) lookup;
	ffloats : (hl_float,as3_float) lookup;
	fidents : (hl_ident,as3_ident) lookup;
	fnamespaces : (hl_namespace,as3_namespace) lookup;
	fnsets : (hl_ns_set,as3_ns_set) lookup;
	fnames : (hl_name,as3_multi_name) lookup;
	fmetas : (hl_metadata,as3_metadata) lookup;
	fmethods : (hl_method,as3_method_type * as3_function,int) gen_lookup;
	fclasses : (hl_class,as3_class * as3_static,hl_name) gen_lookup;
	mutable fjumps : int list;
}

let new_gen_lookup f k =
	{
		h = Hashtbl.create 0;
		a = DynArray.create();
		f = f;
		k = k;
	}

let new_lookup f = new_gen_lookup f (fun x -> x)

let lookup_array l = DynArray.to_array l.a

let lookup ctx (l:('a,'b,'k) gen_lookup) item : 'b index =
	let key = l.k item in
	let idx = try
		Hashtbl.find l.h key
	with Not_found ->
		let idx = DynArray.length l.a in
		(* set dummy value for recursion *)
		DynArray.add l.a (Obj.magic 0);
		Hashtbl.add l.h key (idx + 1);
		DynArray.set l.a idx (l.f ctx item);
		idx + 1
	in
	As3parse.magic_index idx

let lookup_nz ctx l item =
	As3parse.magic_index_nz (As3parse.index_int (lookup ctx l item) - 1)

let lookup_ident ctx i = lookup ctx ctx.fidents i

let lookup_name ctx n = lookup ctx ctx.fnames n

let lookup_method ctx m : as3_method_type index_nz =
	lookup_nz ctx ctx.fmethods m

let lookup_class ctx c : as3_class index_nz =
	lookup_nz ctx ctx.fclasses c

let flatten_namespace ctx = function
	| HNPrivate i -> A3NPrivate (opt lookup_ident ctx i)
	| HNPublic i -> A3NPublic (opt lookup_ident ctx i)
	| HNInternal i -> A3NInternal (opt lookup_ident ctx i)
	| HNProtected i -> A3NProtected (lookup_ident ctx i)
	| HNNamespace i -> A3NNamespace (lookup_ident ctx i)
	| HNExplicit i -> A3NExplicit (lookup_ident ctx i)
	| HNStaticProtected i -> A3NStaticProtected (opt lookup_ident ctx i)

let flatten_ns_set ctx n =
	List.map (lookup ctx ctx.fnamespaces) n

let rec flatten_name ctx = function
	| HMName (i,n) -> A3MName (lookup_ident ctx i,lookup ctx ctx.fnamespaces n)
	| HMMultiName (i,ns) -> A3MMultiName (opt lookup_ident ctx i,lookup ctx ctx.fnsets ns)
	| HMRuntimeName i -> A3MRuntimeName (lookup_ident ctx i)
	| HMRuntimeNameLate -> A3MRuntimeNameLate
	| HMMultiNameLate ns -> A3MMultiNameLate (lookup ctx ctx.fnsets ns)
	| HMAttrib n -> A3MAttrib (flatten_name ctx n)

let flatten_meta ctx m =
	{
		meta3_name = lookup_ident ctx m.hlmeta_name;
		meta3_data = Array.map (fun (i,i2) -> opt lookup_ident ctx i, lookup_ident ctx i2) m.hlmeta_data;
	}

let flatten_value ctx = function
	| HVNone -> A3VNone
	| HVNull -> A3VNull
	| HVBool b -> A3VBool b
	| HVString s -> A3VString (lookup_ident ctx s)
	| HVInt i -> A3VInt (lookup ctx ctx.fints i)
	| HVUInt i -> A3VUInt (lookup ctx ctx.fuints i)
	| HVFloat f -> A3VFloat (lookup ctx ctx.ffloats f)
	| HVNamespace (n,ns) -> A3VNamespace (n,lookup ctx ctx.fnamespaces ns)

let flatten_field ctx f =
	{
		f3_name = lookup_name ctx f.hlf_name;
		f3_slot = f.hlf_slot;
		f3_kind = (match f.hlf_kind with
			| HFMethod m ->
				A3FMethod {
					m3_type = lookup_method ctx m.hlm_type;
					m3_final = m.hlm_final;
					m3_override = m.hlm_override;
					m3_kind = m.hlm_kind;
				}
			| HFVar v ->
				A3FVar {
					v3_type = opt lookup_name ctx v.hlv_type;
					v3_value = flatten_value ctx v.hlv_value;
					v3_const = v.hlv_const;
				}
			| HFFunction f ->
				A3FFunction (lookup_method ctx f)
			| HFClass c ->
				A3FClass (lookup_class ctx c)
		);
		f3_metas = opt (fun ctx -> Array.map (fun m -> lookup_nz ctx ctx.fmetas m)) ctx f.hlf_metas;
	}

let flatten_class ctx c =
	{
		cl3_name = lookup_name ctx c.hlc_name;
		cl3_super = opt lookup_name ctx c.hlc_super;
		cl3_sealed = c.hlc_sealed;
		cl3_final = c.hlc_final;
		cl3_interface = c.hlc_interface;
		cl3_namespace = opt (fun ctx -> lookup ctx ctx.fnamespaces) ctx c.hlc_namespace;
		cl3_implements = Array.map (lookup_name ctx) c.hlc_implements;
		cl3_construct = lookup_method ctx c.hlc_construct;
		cl3_fields = Array.map (flatten_field ctx) c.hlc_fields;
	},
	{
		st3_method = lookup_method ctx c.hlc_static_construct;
		st3_fields = Array.map (flatten_field ctx) c.hlc_static_fields;
	}

let flatten_opcode ctx i = function
	| HBreakPoint -> A3BreakPoint
	| HNop -> A3Nop
	| HThrow -> A3Throw
	| HGetSuper n -> A3GetSuper (lookup_name ctx n)
	| HSetSuper n -> A3SetSuper (lookup_name ctx n)
	| HRegKill r -> A3RegKill r
	| HLabel -> A3Label
	| HJump (j,n) ->
		ctx.fjumps <- i :: ctx.fjumps;
		A3Jump (j,n)
	| HSwitch (n,l) ->
		ctx.fjumps <- i :: ctx.fjumps;
		A3Switch (n,l)
	| HPushWith -> A3PushWith
	| HPopScope -> A3PopScope
	| HForIn -> A3ForIn
	| HHasNext -> A3HasNext
	| HNull -> A3Null
	| HUndefined -> A3Undefined
	| HForEach -> A3ForEach
	| HSmallInt n -> A3SmallInt n
	| HInt n -> A3Int n
	| HTrue -> A3True
	| HFalse -> A3False
	| HNaN -> A3NaN
	| HPop -> A3Pop
	| HDup -> A3Dup
	| HSwap -> A3Swap
	| HString s -> A3String (lookup_ident ctx s)
	| HIntRef i -> A3IntRef (lookup ctx ctx.fints i)
	| HUIntRef i -> A3UIntRef (lookup ctx ctx.fuints i)
	| HFloat f -> A3Float (lookup ctx ctx.ffloats f)
	| HScope -> A3Scope
	| HNamespace n -> A3Namespace (lookup ctx ctx.fnamespaces n)
	| HNext (r1,r2) -> A3Next (r1,r2)
	| HFunction m -> A3Function (lookup_method ctx m)
	| HCallStack n -> A3CallStack n
	| HConstruct n -> A3Construct n
	| HCallMethod (s,n) -> A3CallMethod (s,n)
	| HCallStatic (m,n) -> A3CallStatic (no_nz (lookup_method ctx m),n)
	| HCallSuper (i,n) -> A3CallSuper (lookup_name ctx i,n)
	| HCallProperty (i,n) -> A3CallProperty (lookup_name ctx i,n)
	| HRetVoid -> A3RetVoid
	| HRet -> A3Ret
	| HConstructSuper n -> A3ConstructSuper n
	| HConstructProperty (i,n) -> A3ConstructProperty (lookup_name ctx i,n)
	| HCallPropLex (i,n) -> A3CallPropLex (lookup_name ctx i,n)
	| HCallSuperVoid (i,n) -> A3CallSuperVoid (lookup_name ctx i,n)
	| HCallPropVoid (i,n)-> A3CallPropVoid (lookup_name ctx i,n)
	| HObject n -> A3Object n
	| HArray n -> A3Array n
	| HNewBlock -> A3NewBlock
	| HClassDef c -> A3ClassDef (As3parse.magic_index_nz (As3parse.index_nz_int (lookup_class ctx c)))
	| HCatch n -> A3Catch n
	| HFindPropStrict i -> A3FindPropStrict (lookup_name ctx i)
	| HFindProp i -> A3FindProp (lookup_name ctx i)
	| HFindDefinition i -> A3FindDefinition (lookup_name ctx i)
	| HGetLex i -> A3GetLex (lookup_name ctx i)
	| HSetProp i -> A3SetProp (lookup_name ctx i)
	| HReg r -> A3Reg r
	| HSetReg r -> A3SetReg r
	| HGetGlobalScope -> A3GetGlobalScope
	| HGetScope n -> A3GetScope n
	| HGetProp n -> A3GetProp (lookup_name ctx n)
	| HInitProp n -> A3InitProp (lookup_name ctx n)
	| HDeleteProp n -> A3DeleteProp (lookup_name ctx n)
	| HGetSlot s -> A3GetSlot s
	| HSetSlot s -> A3SetSlot s
	| HToString -> A3ToString
	| HToXml -> A3ToXml
	| HToXmlAttr -> A3ToXmlAttr
	| HToInt -> A3ToInt
	| HToUInt -> A3ToUInt
	| HToNumber -> A3ToNumber
	| HToBool -> A3ToBool
	| HToObject -> A3ToObject
	| HCheckIsXml -> A3CheckIsXml
	| HCast n -> A3Cast (lookup_name ctx n)
	| HAsAny -> A3AsAny
	| HAsString -> A3AsString
	| HAsType n -> A3AsType (lookup_name ctx n)
	| HAsObject -> A3AsObject
	| HIncrReg r -> A3IncrReg r
	| HDecrReg r -> A3DecrReg r
	| HTypeof -> A3Typeof
	| HInstanceOf -> A3InstanceOf
	| HIsType t -> A3IsType (lookup_name ctx t)
	| HIncrIReg r -> A3IncrIReg r
	| HDecrIReg r -> A3DecrIReg r
	| HThis -> A3This
	| HSetThis -> A3SetThis
	| HDebugReg (i,r,l) -> A3DebugReg (lookup_ident ctx i,r,l)
	| HDebugLine l -> A3DebugLine l
	| HDebugFile f -> A3DebugFile (lookup_ident ctx f)
	| HBreakPointLine n -> A3BreakPointLine n
	| HTimestamp -> A3Timestamp
	| HOp op -> A3Op op
	| HUnk c -> A3Unk c

let flatten_code ctx hcode =
	let positions = Array.create (Array.length hcode + 1) 0 in
	let pos = ref 0 in
	let old = ctx.fjumps in
	ctx.fjumps <- [];	
	let code = Array.mapi (fun i op ->
		let op = flatten_opcode ctx i op in
		pos := !pos + As3code.length op;
		Array.set positions (i + 1) !pos;
		op
	) hcode in	
	(* patch jumps *)
	List.iter (fun j ->
		Array.set code j (match Array.get code j with
			| A3Jump (jc,n) -> A3Jump (jc,positions.(j+n) - positions.(j+1))
			| A3Switch (n,infos) -> assert false
			| _ -> assert false);
	) ctx.fjumps;
	ctx.fjumps <- old;
	code

let flatten_try ctx t =
	{
		tc3_start = t.hltc_start;
		tc3_end = t.hltc_end;
		tc3_handle = t.hltc_handle;
		tc3_type = opt lookup_name ctx t.hltc_type;
		tc3_name = opt lookup_name ctx t.hltc_name;
	}

let flatten_method ctx m =
	let mid = lookup_method ctx m in
	let f = m.hlmt_function in
	{
		mt3_ret = opt lookup_name ctx m.hlmt_ret;
		mt3_args = List.map (opt lookup_name ctx) m.hlmt_args;
		mt3_native = m.hlmt_native;
		mt3_var_args = m.hlmt_var_args;
		mt3_arguments_defined = m.hlmt_arguments_defined;
		mt3_uses_dxns = m.hlmt_uses_dxns;
		mt3_new_block = m.hlmt_new_block;
		mt3_unused_flag = m.hlmt_unused_flag;
		mt3_debug_name = opt lookup_ident ctx m.hlmt_debug_name;
		mt3_dparams = opt (fun ctx -> List.map (flatten_value ctx)) ctx m.hlmt_dparams;
		mt3_pnames = opt (fun ctx -> List.map (lookup_ident ctx)) ctx m.hlmt_pnames;
	},
	{
		fun3_id = mid;
		fun3_stack_size = f.hlf_stack_size;
		fun3_nregs = f.hlf_nregs;
		fun3_init_scope = f.hlf_init_scope;
		fun3_max_scope = f.hlf_max_scope;
		fun3_code = flatten_code ctx f.hlf_code;
		fun3_trys = Array.map (flatten_try ctx) f.hlf_trys;
		fun3_locals = Array.map (fun (n,t,s) ->
			{
				f3_name = lookup_name ctx n;
				f3_slot = s;
				f3_kind = A3FVar { v3_type = opt lookup_name ctx t; v3_value = A3VNone; v3_const = false  };
				f3_metas = None;
			}
		) f.hlf_locals;
	}

let flatten_static ctx s =
	{
		st3_method = lookup_method ctx s.hls_method;
		st3_fields = Array.map (flatten_field ctx) s.hls_fields;
	}

let flatten t =
	let id _ x = x in
	let rec ctx = {
		fints = new_lookup id;
		fuints = new_lookup id;
		ffloats = new_lookup id;
		fidents = new_lookup id;
		fnamespaces = new_lookup flatten_namespace;
		fnsets = new_lookup flatten_ns_set;
		fnames = new_lookup flatten_name;
		fmetas = new_lookup flatten_meta;
		fmethods = new_gen_lookup flatten_method (fun m -> m.hlmt_mark);
		fclasses = new_gen_lookup flatten_class (fun c -> c.hlc_name);
		fjumps = [];
	} in
	ignore(lookup_ident ctx "");
	let inits = List.map (flatten_static ctx) t in
	{
		as3_ints = lookup_array ctx.fints;
		as3_uints = lookup_array ctx.fuints;
		as3_floats = lookup_array ctx.ffloats;
		as3_idents = lookup_array ctx.fidents;
		as3_namespaces = lookup_array ctx.fnamespaces;
		as3_nsets = lookup_array ctx.fnsets;
		as3_names = lookup_array ctx.fnames;
		as3_metadatas = lookup_array ctx.fmetas;
		as3_method_types = Array.map fst (lookup_array ctx.fmethods);
		as3_classes = Array.map fst (lookup_array ctx.fclasses);
		as3_statics = Array.map snd (lookup_array ctx.fclasses);
		as3_functions = Array.map snd (lookup_array ctx.fmethods);
		as3_inits = Array.of_list inits;
		as3_unknown = "";
	}
