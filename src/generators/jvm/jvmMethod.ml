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
open Extlib_leftovers
open Globals
open JvmGlobals
open JvmData
open JvmAttribute
open JvmSignature
open JvmSignature.NativeSignatures
open JvmBuilder

let rec pow a b = match b with
	| 0 -> Int32.one
	| 1 -> a
	| _ -> Int32.mul a (pow a (b - 1))

let java_hash s =
	let h = ref Int32.zero in
	let l = UTF8.length s in
	let i31 = Int32.of_int 31 in
	let i = ref 0 in
	UTF8.iter (fun char ->
		let char = Int32.of_int (UCharExt.uint_code char) in
		h := Int32.add !h (Int32.mul char (pow i31 (l - (!i + 1))));
		incr i;
	) s;
	!h

module HashtblList = struct
	type ('a,'b) t = {
		values : ('a,'b) Hashtbl.t;
		mutable keys : 'a list;
	}

	let create () = {
		values = Hashtbl.create 0;
		keys = []
	}

	let add htl key value =
		if not (Hashtbl.mem htl.values key) then begin
			htl.keys <- key :: htl.keys
		end;
		Hashtbl.add htl.values key value

	let as_list htl =
		List.map (fun key ->
			(key,Hashtbl.find_all htl.values key)
		) htl.keys
end

(* High-level method builder. *)

type var_init_state =
	| VarArgument
	| VarWillInit
	| VarNeedDefault

type construction_kind =
	| ConstructInitPlusNew
	| ConstructInit

type label_state =
	| LabelSet of jbranchoffset
	| LabelNotSet of jbranchoffset ref list ref

module NativeArray = struct
	let read code ja je = match je with
		| TBool -> code#baload TBool ja
		| TByte -> code#baload TByte ja
		| TChar -> code#caload ja
		| TDouble -> code#daload ja
		| TFloat -> code#faload ja
		| TInt -> code#iaload ja
		| TLong -> code#laload ja
		| TShort -> code#saload ja
		| _ -> code#aaload ja je

	let write code ja je = match je with
		| TBool -> code#bastore TBool ja
		| TByte -> code#bastore TByte ja
		| TChar -> code#castore ja
		| TDouble -> code#dastore ja
		| TFloat -> code#fastore ja
		| TInt -> code#iastore ja
		| TLong -> code#lastore ja
		| TShort -> code#sastore ja
		| _ -> code#aastore ja je

	let create code pool je =
		let ja = (TArray(je,None)) in
		let primitive i =
			code#newarray ja i
		in
		let reference path =
			let offset = pool#add_path path in
			code#anewarray ja offset;
		in
		begin match je with
		| TBool -> primitive 4
		| TChar -> primitive 5
		| TFloat -> primitive 6
		| TDouble -> primitive 7
		| TByte -> primitive 8
		| TShort -> primitive 9
		| TInt -> primitive 10
		| TLong -> primitive 11
		| TObject(path,_) -> reference path
		| TMethod _ -> reference NativeSignatures.haxe_function_path
		| TTypeParameter _ -> reference NativeSignatures.object_path
		| TArray _ ->
			let offset = pool#add_type (generate_signature false je) in
			code#anewarray ja offset
		| TObjectInner _ | TUninitialized _ -> die "" __LOC__
		end;
		ja
end

class builder jc name jsig = object(self)
	inherit base_builder
	val code = new JvmCode.builder jc#get_pool

	val mutable max_num_locals = 0
	val mutable debug_locals = []
	val mutable stack_frames = []
	val mutable exceptions = []
	val mutable argument_locals = []
	val mutable thrown_exceptions = Hashtbl.create 0
	val mutable closure_count = 0
	val mutable regex_count = 0

	(* per-frame *)
	val mutable locals = []
	val mutable local_offset = 0

	method has_method_flag flag =
		MethodAccessFlags.has_flag access_flags flag

	(** Pushes a new scope onto the stack. Returns a function which when called reverts to the previous state. **)
	method push_scope =
		let old_locals = locals in
		let old_offset = local_offset in
		(fun () ->
			let delta = local_offset - old_offset in
			let fp_end = code#get_fp in
			let rec loop i l =
				if i = 0 then
					()
				else begin match l with
					| (fpo,name,t) :: l ->
						let fp = match !fpo with
							| None -> failwith ("Uninitialized local " ^ name);
							| Some fp -> fp
						in
						let t = match t with
							| TUninitialized None -> TObject(jc#get_this_path,[])
							| _ -> t
						in
						let ld = (fp,fp_end - fp,name,t,old_offset + i - (signature_size t)) in
						debug_locals <- ld :: debug_locals;
						loop (i - (signature_size t)) l
					| [] ->
						die "" __LOC__
				end
			in
			loop delta locals;
			locals <- old_locals;
			local_offset <- old_offset;
		)

	method private get_locals_for_stack_frame locals =
		List.map (fun (init,_,t) ->
			match !init with
			| None -> JvmVerificationTypeInfo.VTop
			| _ -> JvmVerificationTypeInfo.of_signature jc#get_pool t
		) locals

	method get_next_closure_id =
		let id = closure_count in
		closure_count <- closure_count + 1;
		id

	method get_next_regex_id =
		let id = regex_count in
		regex_count <- regex_count + 1;
		id

	(** Adds the current state of locals and stack as a stack frame. This has to be called on every branch target. **)
	method add_stack_frame =
		let locals = self#get_locals_for_stack_frame locals in
		let astack = List.map (JvmVerificationTypeInfo.of_signature jc#get_pool) (code#get_stack#get_stack) in
		let r = code#get_fp in
		let ff = (r,locals,astack) in
		(* If we already have a frame at the same position, overwrite it. This can happen in the case of nested branches. *)
		stack_frames <- (match stack_frames with
			| (r',_,_) :: stack_frames when r' = r -> ff :: stack_frames
			| _ -> ff :: stack_frames)

	(** Adds [exc] as an exception. This will be added to the Code attribute. **)
	method add_exception (exc : jvm_exception) =
		exceptions <- exc :: exceptions

	(** Adds [path] as a thrown exception for this method. Deals with duplicates. **)
	method add_thrown_exception (path : jpath) =
		Hashtbl.replace thrown_exceptions (jc#get_pool#add_path path) true

	(* Convenience *)

	(** Adds [s] as a string constant to the constant pool and emits an instruction to load it. **)
	method string s =
		let offset = jc#get_pool#add_const_string s in
		code#sconst (string_sig) offset

	(** Emits an invokevirtual instruction to invoke method [name] on [path] with signature [jsigm]. **)
	method invokevirtual (path : jpath) (name : string) (jsigm : jsignature) = match jsigm with
		| TMethod(tl,tr) ->
			let offset = code#get_pool#add_field path name jsigm FKMethod in
			code#invokevirtual offset (object_path_sig path) tl (match tr with None -> [] | Some tr -> [tr])
		| _ -> die "" __LOC__

	(** Emits an invokeinterface instruction to invoke method [name] on [path] with signature [jsigm]. **)
	method invokeinterface (path : jpath) (name : string) (jsigm : jsignature) = match jsigm with
		| TMethod(tl,tr) ->
			let offset = code#get_pool#add_field path name jsigm FKInterfaceMethod in
			code#invokeinterface offset (object_path_sig path) tl (match tr with None -> [] | Some tr -> [tr])
		| _ -> die "" __LOC__

	(** Emits an invokespecial instruction to invoke method [name] on [path] with signature [jsigm]. **)
	method invokespecial (path : jpath) (name : string) (jsigm : jsignature) = match jsigm with
		| TMethod(tl,tr) ->
			let offset = code#get_pool#add_field path name jsigm FKMethod in
			code#invokespecial offset (object_path_sig path) tl (match tr with None -> [] | Some tr -> [tr])
		| _ -> die "" __LOC__

	(** Emits an invokestatic instruction to invoke method [name] on [path] with signature [jsigm]. **)
	method invokestatic (path : jpath) (name : string) (jsigm : jsignature) = match jsigm with
		| TMethod(tl,tr) ->
			let offset = code#get_pool#add_field path name jsigm FKMethod in
			code#invokestatic offset tl (match tr with None -> [] | Some tr -> [tr])
		| _ -> die "" __LOC__

	(** Emits a getfield instruction to get the value of field [name] on object [path] with signature [jsigf]. **)
	method getfield (path : jpath) (name : string) (jsigf : jsignature) =
		let offset = code#get_pool#add_field path name jsigf FKField in
		code#getfield offset (object_path_sig path) jsigf

	(** Emits a putfield instruction to set the value of field [name] on object [path] with signature [jsigf]. **)
	method putfield (path : jpath) (name : string) (jsigf : jsignature) =
		let offset = code#get_pool#add_field path name jsigf FKField in
		code#putfield offset (object_path_sig path) jsigf

	(** Emits a getstatic instruction to get the value of field [name] on Class [path] with signature [jsigf]. **)
	method getstatic (path : jpath) (name : string) (jsigf : jsignature) =
		let offset = code#get_pool#add_field path name jsigf FKField in
		code#getstatic offset jsigf

	(** Emits a putstatic instruction to set the value of field [name] on Class [path] with signature [jsigf]. **)
	method putstatic (path : jpath) (name : string) (jsigf : jsignature) =
		let offset = code#get_pool#add_field path name jsigf FKField in
		code#putstatic offset jsigf

	method get_basic_type_class (name : string) =
		self#getstatic (["java";"lang"],name) "TYPE" java_class_sig

	method get_class (jsig : jsignature) =
		match jsig with
		| TByte -> self#get_basic_type_class "Byte"
		| TChar -> self#get_basic_type_class "Character"
		| TDouble -> self#get_basic_type_class "Double"
		| TFloat -> self#get_basic_type_class "Float"
		| TInt -> self#get_basic_type_class "Integer"
		| TLong -> self#get_basic_type_class "Long"
		| TShort -> self#get_basic_type_class "Short"
		| TBool -> self#get_basic_type_class "Boolean"
		| TObject(path,_) ->
			let offset = code#get_pool#add_path path in
			let t = object_path_sig path in
			code#ldc offset (TObject(java_class_path,[TType(WNone,t)]))
		| TTypeParameter _ ->
			let offset = code#get_pool#add_path object_path in
			code#ldc offset (TObject(java_class_path,[TType(WNone,object_sig)]))
		| TArray _ as t ->
			(* TODO: this seems hacky *)
			let offset = code#get_pool#add_path ([],generate_signature false t) in
			code#ldc offset (TObject(java_class_path,[TType(WNone,object_sig)]))
		| TMethod _ ->
			let offset = code#get_pool#add_path haxe_function_path in
			code#ldc offset (TObject(java_class_path,[TType(WNone,object_sig)]))
		| jsig ->
			print_endline (generate_signature false jsig);
			die "" __LOC__

	(** Loads `this` **)
	method load_this =
		code#aload self#get_this_sig 0

	(** Calls the parent constructor with signature [jsig_method] using the specified construction_kind [kind]. **)
	method call_super_ctor (kind : construction_kind) (jsig_method : jsignature) =
		assert (not (self#has_method_flag MStatic));
		match kind with
		| ConstructInitPlusNew ->
			self#invokespecial jc#get_super_path "new" jsig_method;
		| ConstructInit ->
			self#invokespecial jc#get_super_path "<init>" jsig_method;
			self#set_this_initialized

	(** Adds a field named [name] with signature [jsig_field] to the enclosing class, and adds an argument with the same name
	    to this method. The argument value is loaded and stored into the field immediately. **)
	method add_argument_and_field (name : string) (jsig_field : jsignature) (flags : FieldAccessFlags.t list) =
		assert (not (self#has_method_flag MStatic));
		ignore(jc#spawn_field name jsig_field flags);
		let _,load,_ = self#add_local name jsig_field VarArgument in
		self#load_this;
		load();
		self#putfield jc#get_this_path name jsig_field

	(** Constructs a [path] object using the specified construction_kind [kind].

	    The function [f] is invokved to handle the call arguments. Its returned jsignature list is then used as the
		method type of the constructor to invoke.

	    If [no_value] is true, this function ensures that the stack is neutral. Otherwise, the created object is pushed
		onto the stack.
	**)
	method construct ?(no_value=false) (kind : construction_kind) (path : jpath) (f : unit -> jsignature list) =
		let pool = code#get_pool in
		let offset = pool#add_path path in
		code#new_ offset;
		match kind with
		| ConstructInitPlusNew ->
			code#dup;
			code#aconst_null haxe_empty_constructor_sig;
			self#invokespecial path "<init>" (method_sig [haxe_empty_constructor_sig] None);
			if not no_value then self#replace_top (object_path_sig path);
			if not no_value then code#dup;
			let jsigs = f () in
			self#invokevirtual path "new" (method_sig jsigs None);
		| ConstructInit ->
			if not no_value then code#dup;
			let jsigs = f () in
			self#invokespecial path "<init>" (method_sig jsigs None);
			if not no_value then self#replace_top (object_path_sig path)

	(** Loads the default value corresponding to a given signature. **)
	method load_default_value = function
		| TByte | TBool | TChar | TShort | TInt ->
			code#iconst Int32.zero;
		| TFloat -> code#fconst 0.
		| TDouble -> code#dconst 0.
		| TLong -> code#lconst Int64.zero
		| jsig -> code#aconst_null jsig

	(** Constructs a new native array with element type [jsig].

	    Iterates over [fl] and invokes the functions to handle the array elements.
	**)
	method new_native_array (jsig : jsignature) (fl : (unit -> unit) list) =
		code#iconst (Int32.of_int (List.length fl));
		let jasig = NativeArray.create code jc#get_pool jsig in
		List.iteri (fun i f ->
			code#dup;
			code#iconst (Int32.of_int i);
			f();
			self#cast jsig;
			NativeArray.write code jasig jsig
		) fl

	(**
		Emits a return instruction.
	**)
	method return = match jsig with
		| TMethod(_,tr) ->
			begin match tr with
			| None ->
				code#return_void
			| Some jsig ->
				code#return_value jsig
			end;
		| _ ->
			die "" __LOC__

	(* casting *)

	(** Checks if the stack top is a basic type and wraps accordingly. **)
	method expect_reference_type =
		let wrap_null jsig name =
			let path = (["java";"lang"],name) in
			self#invokestatic path "valueOf" (method_sig [jsig] (Some (object_path_sig path)))
		in
		match code#get_stack#top with
		| TByte as t -> wrap_null t "Byte"
		| TChar as t -> wrap_null t "Character"
		| TDouble as t -> wrap_null t "Double"
		| TFloat as t -> wrap_null t "Float"
		| TInt as t -> wrap_null t "Integer"
		| TLong as t -> wrap_null t "Long"
		| TShort as t -> wrap_null t "Short"
		| TBool as t -> wrap_null t "Boolean"
		| _ -> ()

	method private expect_basic_type ?(not_null=false) jsig =
		if not_null then begin
			let unwrap_null tname name =
				let path = (["java";"lang"],tname) in
				self#cast (get_boxed_type jsig);
				self#invokevirtual path name (method_sig [] (Some jsig))
			in
			match jsig with
			| TByte -> unwrap_null "Number" "byteValue"
			| TChar -> unwrap_null "Character" "charValue"
			| TDouble -> unwrap_null "Number" "doubleValue"
			| TFloat -> unwrap_null "Number" "floatValue"
			| TInt -> unwrap_null "Number" "intValue"
			| TLong -> unwrap_null "Number" "longValue"
			| TShort -> unwrap_null "Number" "shortValue"
			| TBool -> unwrap_null "Boolean" "booleanValue"
			| _ -> ()
		end else begin
			let unwrap_null tname name =
				self#invokestatic (["haxe";"jvm"],"Jvm") name (method_sig [object_sig] (Some jsig))
			in
			match jsig with
			| TByte -> unwrap_null "Byte" "toByte"
			| TChar -> unwrap_null "Character" "toChar"
			| TDouble -> unwrap_null "Double" "toDouble"
			| TFloat -> unwrap_null "Float" "toFloat"
			| TInt -> unwrap_null "Integer" "toInt"
			| TLong -> unwrap_null "Long" "toLong"
			| TShort -> unwrap_null "Short" "toShort"
			| TBool -> unwrap_null "Boolean" "toBoolean"
			| _ -> ()
		end

	(** Casts the top of the stack to [jsig]. If [allow_to_string] is true, Jvm.toString is called. **)
	method cast ?(not_null=false) ?(allow_to_string=false) jsig =
		let jsig' = code#get_stack#top in
		let is_number_sig except = function
			| TObject((["java";"lang"],("Byte" | "Short" | "Integer" | "Long" | "Float" | "Double" as name)),_) when name <> except ->
				true
			| _ ->
				false
		in
		let rec unboxed_to_byte () = match code#get_stack#top with
			| TByte | TBool -> ()
			| TChar | TShort | TInt ->
				code#i2b TByte
			| TLong ->
				code#l2i;
				unboxed_to_byte ();
			| TFloat ->
				code#f2i;
				unboxed_to_byte ();
			| TDouble ->
				code#d2i;
				unboxed_to_byte ();
			| jsig ->
				failwith (s_signature_kind jsig);
		in
		let rec unboxed_to_short () = match code#get_stack#top with
			| TShort -> ()
			| TBool | TByte | TChar | TInt ->
				code#i2s;
			| TLong ->
				code#l2i;
				unboxed_to_short ();
			| TFloat ->
				code#f2i;
				unboxed_to_short ();
			| TDouble ->
				code#d2i;
				unboxed_to_short ();
			| _ ->
				die "" __LOC__
		in
		let rec unboxed_to_int () = match code#get_stack#top with
			| TBool | TByte | TShort | TChar | TInt ->
				self#replace_top TInt;
			| TLong ->
				code#l2i;
			| TFloat ->
				code#f2i;
			| TDouble ->
				code#d2i;
			| _ ->
				die "" __LOC__
		in
		let rec unboxed_to_long () = match code#get_stack#top with
			| TBool | TByte | TShort | TChar | TInt ->
				code#i2l;
			| TLong ->
				()
			| TFloat ->
				code#f2l;
			| TDouble ->
				code#d2l;
			| _ ->
				die "" __LOC__
		in
		let rec unboxed_to_float () = match code#get_stack#top with
			| TBool | TByte | TShort | TChar | TInt ->
				code#i2f;
			| TLong ->
				code#l2f;
			| TFloat ->
				()
			| TDouble ->
				code#d2f;
			| _ ->
				die "" __LOC__
		in
		let rec unboxed_to_double () = match code#get_stack#top with
			| TBool | TByte | TShort | TChar | TInt ->
				code#i2d;
			| TLong ->
				code#l2d;
			| TFloat ->
				code#f2d;
			| TDouble ->
				()
			| _ ->
				die "" __LOC__
		in
		let get_conv = function
			| "Byte" -> unboxed_to_byte
			| "Short" -> unboxed_to_short
			| "Integer" -> unboxed_to_int
			| "Long" -> unboxed_to_long
			| "Float" -> unboxed_to_float
			| "Double" -> unboxed_to_double
			| _ -> die "" __LOC__
		in
		let number_to name =
			let boxed_sig = TObject((["java";"lang"],name),[]) in
			self#invokestatic (["haxe";"jvm"],"Jvm") ("numberTo" ^ name) (method_sig [number_sig] (Some boxed_sig))
		in
		let dynamic_to name =
			let boxed_sig = TObject((["java";"lang"],name),[]) in
			self#invokestatic (["haxe";"jvm"],"Jvm") ("dynamicTo" ^ name) (method_sig [object_sig] (Some boxed_sig))
		in
		let numeric_cast_boxed name jsig =
			if is_unboxed jsig then begin
				(get_conv name) ();
				self#expect_reference_type
			end else if is_number_sig name jsig then
				number_to name
			else if is_dynamic_at_runtime jsig then
				dynamic_to name
			else
				code#checkcast (["java";"lang"],name)
		in
		let numeric_cast_unboxed name jsig =
			if is_unboxed jsig then
				(get_conv name) ()
			else begin
				let unboxed_sig = get_unboxed_type (TObject((["java";"lang"],name),[])) in
				self#expect_basic_type unboxed_sig;
				(get_conv name) ()
			end
		in
		begin match jsig,jsig' with
		| TObject((["java";"lang"],"Byte"),_),jsig' ->
			numeric_cast_boxed "Byte" jsig'
		| TByte,jsig' ->
			numeric_cast_unboxed "Byte" jsig'
		| TObject((["java";"lang"],"Short"),_),jsig' ->
			numeric_cast_boxed "Short" jsig'
		| TShort,jsig' ->
			numeric_cast_unboxed "Short" jsig'
		| TObject((["java";"lang"],"Integer"),_),jsig' ->
			numeric_cast_boxed "Integer" jsig'
		| TInt,jsig' ->
			numeric_cast_unboxed "Integer" jsig'
		| TObject((["java";"lang"],"Long"),_),jsig' ->
			numeric_cast_boxed "Long" jsig'
		| TLong,jsig' ->
			numeric_cast_unboxed "Long" jsig'
		| TObject((["java";"lang"],"Float"),_),jsig' ->
			numeric_cast_boxed "Float" jsig'
		| TFloat,jsig' ->
			numeric_cast_unboxed "Float" jsig'
		| TObject((["java";"lang"],"Double"),_),jsig' ->
			numeric_cast_boxed "Double" jsig'
		| TDouble,jsig' ->
			numeric_cast_unboxed "Double" jsig'
		| TChar,TDouble ->
			code#d2i;
			code#i2c;
		| TChar,TFloat ->
			code#f2i;
			code#i2c;
		| TChar,(TByte | TShort | TInt) ->
			code#i2c;
		| TChar,TLong ->
			code#l2i;
			code#i2c;
		| TBool,TInt ->
			self#replace_top TBool;
		| TObject(path1,_),TObject(path2,_) when path1 = path2 ->
			()
		| TObject((["java";"lang"],"String"),_),_ when allow_to_string ->
			self#expect_reference_type;
			self#invokestatic (["haxe";"jvm"],"Jvm") "toString" (method_sig [object_sig] (Some string_sig))
		| TObject(path1,_),t2 ->
			if is_unboxed t2 then
				self#expect_reference_type
			else if path1 = object_path then begin
				(* We should never need a checkcast to Object, but we should adjust the stack so stack maps are wide enough *)
				self#replace_top object_sig
			end else
				code#checkcast path1
		| TMethod _,TMethod _ ->
			()
		| TMethod _,_ ->
			code#checkcast NativeSignatures.haxe_function_path;
		| TArray(jsig1,_),TArray(jsig2,_) when jsig1 = jsig2 ->
			()
		| TArray _,_ ->
			code#checkcast_sig jsig
		| t1,t2 ->
			match is_unboxed t1,is_unboxed t2 with
			| true,false -> self#expect_basic_type ~not_null t1
			| false,true -> self#expect_reference_type
			| _ -> ()
		end

	(* branches *)

	(** Starts a branch. Returns a restore function which reverts the stack and termination status back
	    to the previous state. The restore function can be called multiple times for multiple branches.

		This function has no effect on locals. Use [push_scope] for them.
	**)
	method start_branch =
		let save = code#get_stack#save in
		let old_terminated = code#is_terminated in
		(fun () ->
			code#get_stack#restore save;
			code#set_terminated old_terminated;
		)

	(** Generates code which executes [f_if()] and then branches into [f_then()] and [f_else()]. **)
	method if_then_else (f_if : jbranchoffset ref -> unit) (f_then : unit -> unit) (f_else : unit -> unit) =
		self#if_then_else_labeled (fun label_then label_else ->
			label_else#apply f_if
		) f_then f_else

	method if_then_else_labeled (f_if : label -> label -> unit) (f_then : unit -> unit) (f_else : unit -> unit) =
		let label_then = self#spawn_label "then" in
		let label_else = self#spawn_label "else" in
		let label_exit = self#spawn_label "exit" in
		f_if label_then label_else;
		label_then#here;
		let restore = self#start_branch in
		let pop = self#push_scope in
		f_then();
		pop();
		let term_then = self#is_terminated in
		if not self#is_terminated then label_exit#goto;
		restore();
		label_else#here;
		f_else();
		if term_then && self#is_terminated then
			self#set_terminated true
		else begin
			self#set_terminated false;
			label_exit#here
		end

	(** Generates code which executes [f_if()] and then branches into [f_then()], if the condition holds. **)
	method if_then (f_if : jbranchoffset ref -> unit) (f_then : unit -> unit) =
		self#if_then_labeled (fun _ label_else -> label_else#apply f_if) f_then

	method if_then_labeled (f_if : label -> label -> unit) (f_then : unit -> unit) =
		let label_then = self#spawn_label "then" in
		let label_else = self#spawn_label "else" in
		f_if label_then label_else;
		label_then#here;
		let restore = self#start_branch in
		let pop = self#push_scope in
		f_then();
		pop();
		restore();
		label_else#here

	method spawn_label (name : string) =
		new label (self :> builder) name

	(**
		Returns an instruction offset and emits a goto instruction to it if this method isn't terminated.
	**)
	method maybe_make_jump =
		let r = ref code#get_fp in
		if not self#is_terminated then code#goto r;
		r

	(**
		Closes the list of instruction offsets [rl], effectively making them jump to the current fp.

		Also generates a stack map frame unless [is_exhaustive] is true and all branches terminated.
	**)
	method close_jumps is_exhaustive rl =
		let fp' = code#get_fp in
		let term = List.fold_left (fun term (term',r) ->
			r := fp' - !r;
			term && term'
		) true rl in
		let term = is_exhaustive && term in
		self#set_terminated term;
		if not term then self#add_stack_frame;


	method string_switch
		(need_val : bool)
		(load : (unit -> unit))
		(cases : (string list * (unit -> unit)) list)
		(def : (unit -> unit) option)
	=
		let buckets = HashtblList.create () in
		let exprs = List.mapi (fun index (sl,f) ->
			List.iter (fun s ->
				HashtblList.add buckets (java_hash s) (s,index);
			) sl;
			(f,List.length sl)
		) cases in
		let cases = HashtblList.as_list buckets in
		let exprs = Array.of_list exprs in
		let def = match def with
			| None when need_val ->
				Some (fun () ->
					self#string "Match failure";
					self#invokestatic (["haxe";"jvm"],"Exception") "wrap" (method_sig [object_sig] (Some exception_sig));
					self#get_code#athrow;
				)
			| _ ->
				def
		in
		let label_def = self#spawn_label "default" in
		let label_exit = self#spawn_label "exit" in
		(* all strings can be null and we're not supposed to cause NPEs here... *)
		load();
		label_def#apply (self#get_code#if_null string_sig);
		(* switch *)
		load();
		self#invokevirtual string_path "hashCode" (method_sig [] (Some TInt));
		let exprs = Array.map (fun e -> e,self#spawn_label "case-expr") exprs in
		let jump_table = List.map (fun (hash,l) -> hash,self#spawn_label "hash-match") cases in
		let sorted_jump_table = List.map (fun (hash,label) -> hash,label#mk_offset) jump_table in
		let sorted_jump_table = Array.of_list sorted_jump_table in
		Array.sort (fun (i1,_) (i2,_) -> compare i1 i2) sorted_jump_table;
		code#lookupswitch label_def#mk_offset sorted_jump_table;
		let restore = self#start_branch in
		(* cases *)
		let rec loop cases jumps = match cases,jumps with
			| (_,l) :: cases,(_,label) :: jumps ->
				label#here;
				List.iter (fun (s,i) ->
					restore();
					let pop_scope = self#push_scope in
					let (f,num_jumps),label_expr = exprs.(i) in
					load();
					self#string s;
					self#invokevirtual string_path "equals" (method_sig [object_sig] (Some TBool));
					if num_jumps = 1 then begin
						self#if_then
							(code#if_ CmpEq)
							(fun () ->
								f();
								if not self#is_terminated then label_exit#goto;
							)
					end else
						label_expr#apply (code#if_ CmpNe);
					pop_scope();
				) l;
				label_def#goto;
				loop cases jumps
			| [],[] ->
				()
			| _ ->
				die "" __LOC__
		in
		loop cases jump_table;
		(* exprs *)
		Array.iter (fun ((f,num_jumps),label_expr) ->
			if num_jumps <> 1 then begin
				restore();
				label_expr#here;
				let pop_scope = self#push_scope in
				f();
				pop_scope();
				if not self#is_terminated then label_exit#goto;
			end;
		) exprs;
		(* default *)
		begin match def with
			| None ->
				()
			| Some f ->
				restore();
				label_def#here;
				let pop_scope = self#push_scope in
				f();
				pop_scope();
				if not self#is_terminated then label_exit#goto;
		end;
		if label_exit#was_jumped_to then label_exit#here;
		if def = None then begin
			self#set_terminated false;
			label_def#here;
		end else if label_exit#was_jumped_to then
			self#set_terminated false
		else
			self#set_terminated true


	(**
		Emits a tableswitch or lookupswitch instruction, depending on which one makes more sense.

		The switch subject is expected to be on the stack before calling this function.

		If [is_exhaustive] is true and [def] is None, the first case is used as the default case.
	**)
	method int_switch (need_val : bool) (cases : (Int32.t list * (unit -> unit)) list) (def : (unit -> unit) option) =
		let def = match def with
			| None when need_val ->
				Some (fun () ->
					self#string "Match failure";
					self#invokestatic (["haxe";"jvm"],"Exception") "wrap" (method_sig [object_sig] (Some exception_sig));
					self#get_code#athrow;
				)
			| _ ->
				def
		in
		let flat_cases = DynArray.create () in
		let case_lut = ref Int32Map.empty in
		let imin = ref Int64.max_int in
		let imax = ref Int64.min_int in
		let cases = List.map (fun (il,f) ->
			let rl = List.map (fun i32 ->
				let r = self#spawn_label "case" in
				let i64 = Int64.of_int32 i32 in
				if i64 < !imin then imin := i64;
				if i64 > !imax then imax := i64;
				DynArray.add flat_cases (i32,r#mk_offset);
				case_lut := Int32Map.add i32 r !case_lut;
				r
			) il in
			(rl,f)
		) cases in
		let label_def = self#spawn_label "default" in
		(* No idea what's a good heuristic here... *)
		let diff = Int64.sub !imax !imin in
		let use_tableswitch =
			diff < (Int64.of_int (DynArray.length flat_cases + 10)) &&
			diff >= Int64.zero (* #8388 *)
		in
		if use_tableswitch then begin
			let imin = Int64.to_int32 !imin in
			let imax = Int64.to_int32 !imax in
			let offsets = Array.init (Int32.to_int (Int32.sub imax imin) + 1) (fun i ->
				try Int32Map.find (Int32.add (Int32.of_int i) imin) !case_lut
				with Not_found -> label_def
			) in
			code#tableswitch label_def#mk_offset imin imax (Array.map (fun label -> label#mk_offset) offsets)
		end else begin
			let a = DynArray.to_array flat_cases in
			Array.sort (fun (i1,_) (i2,_) -> compare i1 i2) a;
			code#lookupswitch label_def#mk_offset a;
		end;
		let restore = self#start_branch in
		let label_exit = self#spawn_label "exit" in
		begin match def with
			| None ->
				()
			| Some f ->
				label_def#here;
				let pop_scope = self#push_scope in
				f();
				pop_scope();
				if not self#is_terminated then label_exit#goto;
		end;
		let rec loop cases = match cases with
		| (rl,f) :: cases ->
			restore();
			List.iter (fun label -> label#here) rl;
			let pop_scope = self#push_scope in
			f();
			pop_scope();
			if cases <> [] && not self#is_terminated then label_exit#goto;
			loop cases
		| [] ->
			()
		in
		loop cases;
		if label_exit#was_jumped_to then label_exit#here;
		if def = None then begin
			self#set_terminated false;
			label_def#here;
		end else if label_exit#was_jumped_to then
			self#set_terminated false

	(** Adds a local with a given [name], signature [jsig] and an [init_state].
	    This function returns a tuple consisting of:

		  * The slot of the local
		  * The function to load the value
		  * The function to store a value

		If [init_state = VarNeedDefault], the local is initialized to a default value matching [jsig].
		If [init_state = VarArgument], the local is considered initialized.
		If [init_state = VarWillInit], this function assumes that the returned [store] function will be called appropriately.
	**)
	method add_local (name : string) (jsig : jsignature) (init_state : var_init_state) =
		let slot = local_offset in
		let load,store,d = match jsig with
			| TInt | TBool | TByte | TShort | TChar ->
				if init_state = VarNeedDefault then begin
					code#iconst Int32.zero;
					code#istore slot
				end;
				(fun () -> code#iload ~jsig slot),(fun () -> code#istore slot),1
			| TLong ->
				if init_state = VarNeedDefault then begin
					code#lconst Int64.zero;
					code#lstore slot
				end;
				(fun () -> code#lload slot),(fun () -> code#lstore slot),2
			| TFloat ->
				if init_state = VarNeedDefault then begin
					code#fconst 0.;
					code#fstore slot
				end;
				(fun () -> code#fload slot),(fun () -> code#fstore slot),1
			| TDouble ->
				if init_state = VarNeedDefault then begin
					code#dconst 0.;
					code#dstore slot
				end;
				(fun () -> code#dload slot),(fun () -> code#dstore slot),2
			| _ ->
				if init_state = VarNeedDefault then begin
					code#aconst_null jsig;
					code#astore jsig slot
				end;
				(fun () -> code#aload jsig slot),(fun () -> code#astore jsig slot),1
		in
		let init = ref None in
		locals <- (init,name,jsig) :: locals;
		local_offset <- local_offset + d;
		if local_offset > max_num_locals then max_num_locals <- local_offset;
		let check_store =
			let did_store = ref false in
			(fun () ->
				if not !did_store then begin
					did_store := true;
					init := Some (code#get_fp)
				end
			)
		in
		begin match init_state with
		| VarArgument | VarNeedDefault -> check_store();
		| _ -> ()
		end;
		slot,
		load,
		(fun () ->
			store();
			check_store();
		)

	method get_this_sig =
		let rec loop locals = match locals with
			| [(_,_,jsig)] -> jsig
			| _ :: locals -> loop locals
			| [] -> die "" __LOC__
		in
		loop locals

	method set_this_initialized =
		let rec loop acc locals = match locals with
			| [(init,name,_)] -> List.rev ((init,name,jc#get_jsig) :: acc)
			| [] -> die "" __LOC__
			| l :: locals -> loop (l :: acc) locals
		in
		locals <- loop [] locals

	method replace_top jsig =
		code#get_stack#replace jsig

	(** This function has to be called once all arguments are declared. *)
	method finalize_arguments =
		argument_locals <- locals

	method private get_stack_map_table =
		let argument_locals = self#get_locals_for_stack_frame argument_locals in
		let stack_map = List.fold_left (fun ((last_offset,last_locals,last_locals_length),acc) (offset,locals,stack) ->
			let cur = offset - last_offset - 1 in
			let a_locals = Array.of_list (List.rev locals) in
			let locals_length = Array.length a_locals in
			let default () =
				StackFull(cur,a_locals,Array.of_list (List.rev stack))
			in
			let entry = match stack,locals_length - last_locals_length with
			| [],0 ->
				if last_locals = locals then begin
					if cur < 64 then StackSame cur
					else StackSameExtended cur
				end else
					default()
			| [vt],0 ->
				if last_locals = locals then begin
					if cur < 64 then Stack1StackItem(cur,vt)
					else Stack1StackItemExtended(cur,vt)
				end else
					default()
			| [],1 ->
				begin match locals with
				| vt1 :: locals when locals = last_locals -> StackAppend1(cur,vt1)
				| _ -> default()
				end
			| [],2 ->
				begin match locals with
				| vt1 :: vt2 :: locals when locals = last_locals -> StackAppend2(cur,vt2,vt1)
				| _ -> default()
				end
			| [],3 ->
				begin match locals with
				| vt1 :: vt2 :: vt3 :: locals when locals = last_locals -> StackAppend3(cur,vt3,vt2,vt1)
				| _ -> default()
				end
			| [],-1 ->
				begin match last_locals with
				| _ :: last_locals when locals = last_locals -> StackChop1 cur
				| _ -> default()
				end
			| [],-2 ->
				begin match last_locals with
				| _ :: _ :: last_locals when locals = last_locals -> StackChop2 cur
				| _ -> default()
				end
			| [],-3 ->
				begin match last_locals with
				| _ :: _ :: _ :: last_locals when locals = last_locals -> StackChop3 cur
				| _ -> default()
				end
			| _ ->
				default()
			in
			((offset,locals,locals_length),entry :: acc)
		) ((-1,argument_locals,List.length argument_locals),[]) (List.rev stack_frames) in
		Array.of_list (List.rev (snd stack_map))

	method get_code = code
	method is_terminated = code#is_terminated
	method get_name = name
	method get_jsig = jsig
	method set_terminated b = code#set_terminated b

	method private get_jcode (config : export_config) =
		let attributes = DynArray.create () in
		let lines = code#get_lines in
		if config.export_debug && DynArray.length lines > 0 then
			DynArray.add attributes (AttributeLineNumberTable (DynArray.to_array lines));
		let stack_map_table = self#get_stack_map_table in
		if Array.length stack_map_table > 0 then
			DynArray.add attributes (AttributeStackMapTable stack_map_table);
		let exceptions = Array.of_list (List.rev exceptions) in
		if config.export_debug then begin match debug_locals with
		| [] ->
			()
		| _ ->
			let type_locals = DynArray.create () in
			let map (fp,length,name,jsig,index) =
				let ld = {
					ld_start_pc = fp;
					ld_length = length;
					ld_name_index = jc#get_pool#add_string name;
					ld_descriptor_index = jc#get_pool#add_string (generate_signature false jsig);
					ld_index = index;
				} in
				if has_type_parameter jsig then DynArray.add type_locals {ld with ld_descriptor_index = jc#get_pool#add_string (generate_signature true jsig)};
				ld
			in
			let locals = Array.of_list (List.map map debug_locals) in
			DynArray.add attributes (AttributeLocalVariableTable locals);
			if DynArray.length type_locals > 0 then begin
				let locals = DynArray.to_array type_locals in
				DynArray.add attributes (AttributeLocalVariableTypeTable locals);
			end
		end;
		let attributes = List.map (JvmAttribute.write_attribute jc#get_pool) (DynArray.to_list attributes) in
		{
			code_max_stack = code#get_max_stack_size;
			code_max_locals = max_num_locals;
			code_code = code#export_code;
			code_exceptions = exceptions;
			code_attributes = Array.of_list attributes;
		}

	(** Exports the method as a [jvm_field]. No other functions should be called on this object afterwards. *)
	method export_method (config : export_config) =
		assert (not was_exported);
		was_exported <- true;
		self#commit_annotations jc#get_pool;
		if code#get_fp > 0 then begin
			let code = self#get_jcode config in
			self#add_attribute (AttributeCode code);
		end;
		if Hashtbl.length thrown_exceptions > 0 then
			self#add_attribute (AttributeExceptions (Array.of_list (Hashtbl.fold (fun k _ c -> k :: c) thrown_exceptions [])));
		let attributes = self#export_attributes jc#get_pool in
		let offset_name = jc#get_pool#add_string name in
		let jsig = generate_method_signature false jsig in
		let offset_desc = jc#get_pool#add_string jsig in
		{
			field_access_flags = access_flags;
			field_name_index = offset_name;
			field_descriptor_index = offset_desc;
			field_attributes = attributes;
		}

	(** Exports the method as a [jvm_field]. No other functions should be called on this object afterwards. *)
	method export_field =
		assert (code#get_fp = 0);
		assert (not was_exported);
		was_exported <- true;
		let attributes = self#export_attributes jc#get_pool in
		let offset_name = jc#get_pool#add_string name in
		let jsig = generate_signature false jsig in
		let offset_desc = jc#get_pool#add_string jsig in
		{
			field_access_flags = access_flags;
			field_name_index = offset_name;
			field_descriptor_index = offset_desc;
			field_attributes = attributes;
		}
end

and label (jm : builder) (name : string) = object(self)

	val code = jm#get_code

	val mutable state = LabelNotSet (ref [])
	val mutable was_jumped_to = false

	method was_jumped_to = was_jumped_to

	method get_offset = match state with
		| LabelSet fp -> fp
		| LabelNotSet _ -> failwith (Printf.sprintf "Trying to get offset of unset label %s" name)

	method mk_offset =
		let r = ref code#get_fp in
		was_jumped_to <- true;
		begin match state with
		| LabelNotSet l ->
			l := r :: !l
		| LabelSet fp' ->
			r := fp' - !r
		end;
		r

	method apply (f : jbranchoffset ref -> unit) =
		f self#mk_offset

	method if_ (cmp : jcmp) =
		code#if_ cmp self#mk_offset

	method if_null jsig =
		code#if_null jsig self#mk_offset

	method if_nonnull jsig =
		code#if_nonnull jsig self#mk_offset

	method goto =
		code#goto self#mk_offset

	method at fp = match state with
		| LabelNotSet l ->
			if fp = code#get_fp then jm#add_stack_frame;
			List.iter (fun r ->
				r := fp - !r
			) !l;
			state <- LabelSet fp
		| LabelSet _ ->
			if fp <> self#get_offset then
				failwith (Printf.sprintf "Trying to instantiate label %s again" name)

	method here =
		self#at code#get_fp
end