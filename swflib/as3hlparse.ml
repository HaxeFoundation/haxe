open As3
open As3hl

type parse_ctx = {
	as3 : as3_tag;
	mutable namespaces : hl_namespace array;
	mutable nsets : hl_ns_set array;
	mutable names : hl_name array;
	mutable methods : hl_method array;
	mutable classes : hl_class array;
}

let get = As3parse.iget
let no_nz = As3parse.no_nz
let idx n = As3parse.index_int n - 1

let ident ctx i = get ctx.as3.as3_idents i
let name ctx n = ctx.names.(idx n)
let method_type ctx n = ctx.methods.(idx (no_nz n))
let getclass ctx n = ctx.classes.(idx (no_nz n))

let opt f ctx = function
	| None -> None
	| Some x -> Some (f ctx x)

let parse_opcode ctx = function
	| A3BreakPoint -> HBreakPoint
	| A3Nop -> HNop
	| A3Throw -> HThrow
	| A3GetSuper n -> HGetSuper (name ctx n)
	| A3SetSuper n -> HSetSuper (name ctx n)
	| A3RegKill r -> HRegKill r
	| A3Label -> HLabel
	| A3Jump (j,n) -> HJump (j,n)
	| A3Switch (n,infos) -> HSwitch(n,infos)
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
		hlf_metas = match f.f3_metas with
			| None -> None
			| Some a -> Some (Array.map (fun i -> parse_metadata ctx (get ctx.as3.as3_metadatas (no_nz i))) a);
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
		hlf_code = []; (* keep for later *)
		hlf_trys = Array.map (parse_try_catch ctx) f.fun3_trys;
		hlf_locals = Array.mapi (fun i f ->
			(* locals are supposed to be ordoned *)
			if f.f3_slot <> i then assert false;
			if f.f3_metas <> None then assert false;
			match f.f3_kind with
			| A3FVar v ->
				if v.v3_const || v.v3_value <> A3VNone then assert false;
				name ctx f.f3_name , opt name ctx v.v3_type
			| _ -> assert false
		) f.fun3_locals;
	}

let parse_method_type ctx m f =
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
		m.hlmt_function.hlf_code <- List.map (parse_opcode ctx) f.fun3_code;
	) t.as3_functions;
	inits
