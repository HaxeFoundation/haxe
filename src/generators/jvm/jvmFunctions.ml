open JvmSignature
open NativeSignatures

type signature_classification =
	| CByte
	| CChar
	| CDouble
	| CFloat
	| CInt
	| CLong
	| CShort
	| CBool
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
		let suffix = Option.map_default string_of_classification "Void" cr in
		let meth = {
			arity = List.length cl;
			name = Printf.sprintf "invoke%s" suffix;
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
				assert false
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
				jm#invokevirtual jc#get_this_path "invokeObject" (method_sig args (Some object_sig));
				jm#return;
			)
		) in
		let def = (fun () ->
			jm#string "Invalid call";
			jm#invokestatic (["haxe";"jvm"],"Exception") "wrap" (method_sig [object_sig] (Some exception_sig));
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
			let jm = jc#spawn_method "invokeObject" jsig [MPublic] in
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
		jm_ctor#add_argument_and_field "func" haxe_function_sig;
		jm_ctor#finalize_arguments;
		jm_ctor#load_this;
		jm_ctor#call_super_ctor ConstructInit (method_sig [] None);
		jm_ctor#return;
		let rec loop args i =
			let jsig = method_sig args (Some object_sig) in
			let jm = jc#spawn_method "invokeObject" jsig [MPublic] in
			let vars = ExtList.List.init i (fun i ->
				jm#add_local (Printf.sprintf "arg%i" i) object_sig VarArgument
			) in
			jm#load_this;
			jm#getfield jc#get_this_path "func" haxe_function_sig;
			jm#new_native_array object_sig (List.map (fun (_,load,_) () -> load()) vars);
			jm#invokestatic (["haxe";"root"],"Array") "ofNative" (method_sig [array_sig object_sig] (Some (object_path_sig (["haxe";"root"],"Array"))));
			jm#invokevirtual haxe_function_path "invokeObject" (method_sig [object_sig] (Some object_sig));
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
			let jm = jc#spawn_method meth.name (method_sig meth.dargs meth.dret) [MPublic] in
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


class typed_function
	(functions : typed_functions)
	(host_class : JvmClass.builder)
	(host_method : JvmMethod.builder)
	(context : (string * jsignature) list)

= object(self)

	val jc_closure =
		let jc = host_class#spawn_inner_class None haxe_function_path (Some host_class#get_next_closure_name) in
		jc#add_access_flag 0x10; (* final *)
		jc

	method get_class = jc_closure

	method generate_constructor (public : bool) =
		let context_sigs = List.map snd context in
		let jm_ctor = jc_closure#spawn_method "<init>" (method_sig context_sigs None) (if public then [MPublic] else []) in
		List.iter (fun (name,jsig) ->
			jm_ctor#add_argument_and_field name jsig;
		) context;
		jm_ctor#load_this;
		jm_ctor#call_super_ctor ConstructInit (method_sig [] None);
		jm_ctor#return;
		jm_ctor

	method generate_invoke (args : (string * jsignature) list) (ret : jsignature option)=
		let arg_sigs = List.map snd args in
		let meth = functions#register_signature arg_sigs ret in
		let jsig_invoke = method_sig arg_sigs ret in
		let jm_invoke = jc_closure#spawn_method meth.name jsig_invoke [MPublic] in
		let rec loop meth =
			begin match meth.next with
			| Some meth_next ->
				let jm_invoke_next = jc_closure#spawn_method meth_next.name (method_sig meth_next.dargs meth_next.dret) [MPublic] in
				functions#make_forward_method jc_closure jm_invoke_next meth_next meth;
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