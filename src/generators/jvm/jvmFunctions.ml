open JvmGlobals.MethodAccessFlags
open JvmSignature
open NativeSignatures

type signature_classification =
	| CBool
	| CByte
	| CChar
	| CShort
	| CInt
	| CLong
	| CFloat
	| CDouble
	| CObject

type method_signature = {
	arity : int;
	name : string;
	has_nonobject : bool;
	sort_string : string;
	cargs : signature_classification list;
	cret : signature_classification option;
	dargs : jsignature list;
	dret : jsignature option;
	mutable next : method_signature option;
}

let string_of_classification = function
	| CByte -> "Byte"
	| CChar -> "Char"
	| CDouble -> "Double"
	| CFloat -> "Float"
	| CInt -> "Int"
	| CLong -> "Long"
	| CShort -> "Short"
	| CBool -> "Bool"
	| CObject -> "Object"

let classify = function
	| TByte -> CByte
	| TChar -> CChar
	| TDouble -> CDouble
	| TFloat -> CFloat
	| TInt -> CInt
	| TLong -> CLong
	| TShort -> CShort
	| TBool -> CBool
	| TObject _
	| TObjectInner _
	| TArray _
	| TMethod _
	| TTypeParameter _
	| TUninitialized _ -> CObject

let declassify = function
	| CByte -> TByte
	| CChar -> TChar
	| CDouble -> TDouble
	| CFloat -> TFloat
	| CInt -> TInt
	| CLong -> TLong
	| CShort -> TShort
	| CBool -> TBool
	| CObject -> object_path_sig object_path

class typed_functions = object(self)
	val signatures = Hashtbl.create 0
	val mutable max_arity = 0

	method register_signature (tl : jsignature list) (tr : jsignature option) =
		let cl = List.map classify tl in
		let cr = Option.map classify tr in
		self#get_signature cl cr

	method objectify (meth : method_signature) =
		let cl_objects = List.map (fun _ -> CObject) meth.cargs in
		self#get_signature cl_objects meth.cret

	method private get_signature
		(cl : signature_classification list)
		(cr : signature_classification option)
	=
		try
			Hashtbl.find signatures (cl,cr)
		with Not_found ->
			self#do_register_signature cl cr

	method private do_register_signature
		(cl : signature_classification list)
		(cr : signature_classification option)
	=
		let to_string (cl,cr) =
			Printf.sprintf "[%s] %s"
				(String.concat ", " (List.map string_of_classification cl))
				(Option.map_default string_of_classification "CVoid" cr)
		in
		let meth = {
			arity = List.length cl;
			name = "invoke";
			has_nonobject = List.exists (function CObject -> false | _ -> true) cl;
			sort_string = to_string (cl,cr);
			cargs = cl;
			cret = cr;
			dargs = List.map declassify cl;
			dret = Option.map declassify cr;
			next = None;
		} in
		if meth.arity > max_arity then max_arity <- meth.arity;
		Hashtbl.add signatures (meth.cargs,meth.cret) meth;
		(* If the method has something that's not java.lang.Object, the next method is one where all arguments are
		   of type java.lang.Object. *)
		if meth.has_nonobject then begin
			let meth_objects = self#objectify meth in
			meth.next <- Some meth_objects;
		(* Otherwise, if the method has a return type that's not java.lang.Object, the next method is one that returns
		   java.lang.Object. *)
		end else begin match cr with
			| Some CObject ->
				()
			| _ ->
				meth.next <- Some (self#get_signature meth.cargs (Some CObject))
		end;
		meth

	method make_forward_method
		(jc : JvmClass.builder)
		(jm : JvmMethod.builder)
		(meth_from : method_signature)
		(meth_to : method_signature)
	=
		let args = List.mapi (fun i jsig ->
			jm#add_local (Printf.sprintf "arg%i" i) jsig VarArgument
		) meth_from.dargs in
		jm#finalize_arguments;
		jm#load_this;
		let rec loop loads jsigs = match loads,jsigs with
			| (_,load,_) :: loads,jsig :: jsigs ->
				load();
				jm#cast jsig;
				loop loads jsigs
			| [],jsig :: jsigs ->
				jm#load_default_value jsig;
				loop [] jsigs
			| [],[] ->
				()
			| _,[] ->
				Globals.die "" __LOC__
		in
		loop args meth_to.dargs;
		jm#invokevirtual jc#get_this_path meth_to.name (method_sig meth_to.dargs meth_to.dret);
		begin match meth_from.dret,meth_to.dret with
		| None,None ->
			()
		| Some jsig,Some _ ->
			jm#cast jsig;
		| None,Some jsig ->
			jm#get_code#pop
		| Some jsig,None ->
			jm#load_default_value jsig;
		end;
		jm#return;

	method generate_invoke_dynamic (jc : JvmClass.builder) =
		let array_sig = TArray(object_sig,None) in
		let jm = jc#spawn_method "invokeDynamic" (method_sig [array_sig] (Some object_sig)) [MPublic] in
		let _,load,_ = jm#add_local "args" array_sig VarArgument in
		jm#finalize_arguments;
		load();
		jm#get_code#arraylength array_sig;
		let cases = ExtList.List.init max_arity (fun i ->
			[Int32.of_int i],(fun () ->
				jm#load_this;
				let args = ExtList.List.init i (fun index ->
					load();
					jm#get_code#iconst (Int32.of_int index);
					jm#get_code#aaload array_sig object_sig;
					object_sig
				) in
				jm#invokevirtual jc#get_this_path "invoke" (method_sig args (Some object_sig));
				jm#return;
			)
		) in
		let def = (fun () ->
			jm#construct ConstructInit (["java";"lang"],"IllegalArgumentException") (fun () -> []);
			jm#get_code#athrow;
			jm#set_terminated true;
		) in
		ignore(jm#int_switch true cases (Some def));

	method generate_closure_dispatch =
		let jc = new JvmClass.builder (["haxe";"jvm"],"ClosureDispatch") haxe_function_path in
		jc#add_access_flag 1; (* public *)
		let jm_ctor = jc#spawn_method "<init>" (method_sig [] None) [MPublic] in
		jm_ctor#finalize_arguments;
		jm_ctor#load_this;
		jm_ctor#call_super_ctor ConstructInit (method_sig [] None);
		jm_ctor#return;
		let rec loop args i =
			let jsig = method_sig args (Some object_sig) in
			let jm = jc#spawn_method "invoke" jsig [MPublic] in
			let vars = ExtList.List.init i (fun i ->
				jm#add_local (Printf.sprintf "arg%i" i) object_sig VarArgument
			) in
			jm#load_this;
			jm#new_native_array object_sig (List.map (fun (_,load,_) () -> load()) vars);
			jm#invokevirtual haxe_function_path "invokeDynamic" (method_sig [array_sig object_sig] (Some object_sig));
			jm#return;
			if i < max_arity then loop (object_sig :: args) (i + 1)
		in
		loop [] 0;
		jc

	method generate_var_args =
		let jc = new JvmClass.builder (["haxe";"jvm"],"VarArgs") haxe_function_path in
		jc#add_access_flag 1; (* public *)
		let jm_ctor = jc#spawn_method "<init>" (method_sig [haxe_function_sig] None) [MPublic] in
		jm_ctor#add_argument_and_field "func" haxe_function_sig [FdPublic;FdFinal];
		jm_ctor#finalize_arguments;
		jm_ctor#load_this;
		jm_ctor#call_super_ctor ConstructInit (method_sig [] None);
		jm_ctor#return;
		let rec loop args i =
			let jsig = method_sig args (Some object_sig) in
			let jm = jc#spawn_method "invoke" jsig [MPublic;MBridge;MSynthetic] in
			let vars = ExtList.List.init i (fun i ->
				jm#add_local (Printf.sprintf "arg%i" i) object_sig VarArgument
			) in
			jm#load_this;
			jm#getfield jc#get_this_path "func" haxe_function_sig;
			jm#new_native_array object_sig (List.map (fun (_,load,_) () -> load()) vars);
			jm#invokestatic (["haxe";"root"],"Array") "ofNative" (method_sig [array_sig object_sig] (Some (object_path_sig (["haxe";"root"],"Array"))));
			jm#invokevirtual haxe_function_path "invoke" (method_sig [object_sig] (Some object_sig));
			jm#return;
			if i < max_arity then loop (object_sig :: args) (i + 1)
		in
		loop [] 0;
		jc

	method generate =
		let l = Hashtbl.fold (fun _ v acc -> v :: acc) signatures [] in
		let l = List.sort (fun meth1 meth2 -> compare (meth1.arity,meth1.sort_string) (meth2.arity,meth2.sort_string)) l in
		let jc = new JvmClass.builder haxe_function_path object_path in
		jc#add_access_flag 1; (* public *)
		List.iter (fun meth ->
			let jm = jc#spawn_method meth.name (method_sig meth.dargs meth.dret) [MPublic;MBridge;MSynthetic] in
			begin match meth.next with
			| Some meth_next ->
				self#make_forward_method jc jm meth meth_next;
			| None when meth.arity < max_arity && not meth.has_nonobject ->
				let meth_next = self#get_signature (CObject :: meth.cargs) meth.cret in
				self#make_forward_method jc jm meth meth_next
			| None ->
				List.iteri (fun i jsig ->
					ignore(jm#add_local (Printf.sprintf "arg%i" i) jsig VarArgument)
				) meth.dargs;
				jm#finalize_arguments;
				begin match meth.dret with
				| Some jsig -> jm#load_default_value jsig
				| None -> ()
				end;
				jm#return;
			end;
		) l;
		let jm_ctor = jc#spawn_method "<init>" (method_sig [] None) [MPublic] in
		jm_ctor#load_this;
		jm_ctor#call_super_ctor ConstructInit (method_sig [] None);
		jm_ctor#return;
		self#generate_invoke_dynamic jc;
		jc
end

type typed_function_kind =
	| FuncLocal
	| FuncMember of jpath * string
	| FuncStatic of jpath * string

module JavaFunctionalInterfaces = struct
	type t = {
		jargs: jsignature list;
		jret : jsignature option;
		jarity : int;
		jpath : jpath;
		jname : string;
		jparams : string list;
	}

	let s_functional_interface fi = Type.Printer.s_record_fields "" [
		"jargs",String.concat ", " (List.map s_signature_kind fi.jargs);
		"jret",Option.map_default s_signature_kind "None" fi.jret;
		"jarity",string_of_int fi.jarity;
		"jpath",Globals.s_type_path fi.jpath;
		"jname",fi.jname;
		"jparams",String.concat ", " fi.jparams;
	]

	let java_functional_interfaces = DynArray.create ()

	let add_functional_interface path jsig name params =
		let args,ret = match jsig with
			| TMethod(args,ret) -> args,ret
			| _ -> Globals.die "" __LOC__
		in
		let fi = {
			jargs = args;
			jret = ret;
			jarity = List.length args;
			jpath = path;
			jname = name;
			jparams = params
		} in
		while DynArray.length java_functional_interfaces <= fi.jarity do
			DynArray.add java_functional_interfaces (DynArray.create ())
		done;
		DynArray.add (DynArray.get java_functional_interfaces fi.jarity) fi

	let unify jfi args ret =
		let unify_sig jsig1 jsig2 =
			jsig1 = jsig2 || match jsig1 with
				| TObject(path,_) when path = object_path ->
					(* TODO: This still isn't quite right. We technically have to determine a proper unification here, but that isn't really possible
					   with just signatures... *)
					not (is_unboxed jsig2)
				| _ ->
					false
		in
		let rec loop params want have = match want,have with
			| [],[] ->
				Some (jfi,List.map (fun s -> TType(WNone,List.assoc s params)) jfi.jparams)
			| want1 :: want,have1 :: have ->
				begin match want1 with
				| TTypeParameter n ->
					let have1 = get_boxed_type have1 in
					loop ((n,have1) :: params) want have
				| _ ->
					(* contravariance! *)
					if unify_sig want1 have1 then loop params want have else None
				end
			| _ ->
				None
		in
		match jfi.jret,ret with
		| None,None ->
			loop [] jfi.jargs args
		| Some (TTypeParameter n),Some jsig ->
			let jsig = get_boxed_type jsig in
			loop [n,jsig] jfi.jargs args
		| Some jsig1,Some jsig2 ->
			if unify_sig jsig1 jsig2 then loop [] jfi.jargs args else None
		| _ ->
			None

	let find_compatible args ret filter =
		let arity = List.length args in
		if arity >= DynArray.length java_functional_interfaces then
			[]
		else begin
			let l = ref [] in
			DynArray.iter (fun jfi ->
				let attempt () = match unify jfi args ret with
				| None ->
					()
				| Some r ->
					l := r :: !l
				in
				match filter with
				| [] ->
					attempt()
				| l when List.mem jfi.jpath l ->
					attempt()
				| _ ->
					()
			) (DynArray.get java_functional_interfaces arity);
			!l
		end
end

open JavaFunctionalInterfaces
open JvmGlobals

class typed_function
	(functions : typed_functions)
	(kind : typed_function_kind)
	(host_class : JvmClass.builder)
	(host_method : JvmMethod.builder)
	(context : (string * jsignature) list)

= object(self)

	val jc_closure =
		let name = match kind with
			| FuncLocal ->
				Printf.sprintf "Closure_%s_%i" (patch_name host_method#get_name) host_method#get_next_closure_id
			| FuncStatic(path,name) ->
				Printf.sprintf "%s_%s" (snd path) (patch_name name)
			| FuncMember(path,name) ->
				Printf.sprintf "%s_%s" (snd path) (patch_name name)
		in
		let jc = host_class#spawn_inner_class None haxe_function_path (Some name) in
		jc#add_access_flag 0x10; (* final *)
		jc

	method get_class = jc_closure

	method generate_constructor (public : bool) =
		let context_sigs = List.map snd context in
		let jm_ctor = jc_closure#spawn_method "<init>" (method_sig context_sigs None) (if public then [MPublic] else []) in
		List.iter (fun (name,jsig) ->
			jm_ctor#add_argument_and_field name jsig [FdPublic;FdFinal];
		) context;
		jm_ctor#load_this;
		jm_ctor#call_super_ctor ConstructInit (method_sig [] None);
		jm_ctor#return;
		jm_ctor

	method generate_invoke (args : (string * jsignature) list) (ret : jsignature option) (functional_interface_filter : jpath list) =
		let arg_sigs = List.map snd args in
		let meth = functions#register_signature arg_sigs ret in
		let jsig_invoke = method_sig arg_sigs ret in
		let jm_invoke = jc_closure#spawn_method meth.name jsig_invoke [MPublic] in
		let implemented_interfaces = Hashtbl.create 0 in
		let add_interface path params =
			if not (Hashtbl.mem implemented_interfaces path) then begin
				jc_closure#add_interface path params;
				Hashtbl.add implemented_interfaces path true;
			end
		in
		let spawn_forward_function meth_from meth_to is_bridge =
			let jsig = method_sig meth_from.dargs meth_from.dret in
			if not (jc_closure#has_method meth_from.name jsig) then begin
				let flags = [MPublic] in
				let flags = if is_bridge then MBridge :: MSynthetic :: flags else flags in
				let jm_invoke_next = jc_closure#spawn_method meth_from.name jsig flags in
				functions#make_forward_method jc_closure jm_invoke_next meth_from meth_to;
			end
		in
		let check_functional_interfaces meth =
			try
				let l = JavaFunctionalInterfaces.find_compatible meth.dargs meth.dret functional_interface_filter in
				List.iter (fun (jfi,params) ->
					add_interface jfi.jpath params;
					spawn_forward_function {meth with name=jfi.jname} meth false;
				) l
			with Not_found ->
				()
		in
		let rec loop meth =
			check_functional_interfaces meth;
			begin match meth.next with
			| Some meth_next ->
				spawn_forward_function meth_next meth true;
				loop meth_next;
			| None ->
				()
			end;
		in
		let return_differs = match meth.dret,ret with
			| None,None -> false
			| Some jsig1,Some jsig2 -> not (equals_at_runtime jsig1 jsig2)
			| _ -> true
		in
		let meth = if not (List.for_all2 equals_at_runtime meth.dargs arg_sigs) || return_differs then begin
			let meth_prev = meth in
			let meth = {meth with dargs = arg_sigs; dret = ret} in
			meth.next <- Some meth_prev;
			meth
		end else
			meth
		in
		loop meth;
		jm_invoke
end