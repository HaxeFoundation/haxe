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

open Globals
open JvmGlobals
open JvmData
open JvmAttribute
open JvmSignature
open JvmSignature.NativeSignatures
open JvmBuilder

(* High-level method builder. *)

type var_init_state =
	| VarArgument
	| VarWillInit
	| VarNeedDefault

type construction_kind =
	| ConstructInitPlusNew
	| ConstructInit

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
		| TMethod _ -> reference NativeSignatures.method_handle_path
		| TTypeParameter _ -> reference NativeSignatures.object_path
		| TArray _ ->
			let offset = pool#add_type (generate_signature false je) in
			code#anewarray ja offset
		| TObjectInner _ | TUninitialized _ -> assert false
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

	(* per-branch *)
	val mutable terminated = false

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
						let ld = {
							ld_start_pc = fp;
							ld_length = fp_end - fp;
							ld_name_index = jc#get_pool#add_string name;
							ld_descriptor_index = jc#get_pool#add_string (generate_signature false t);
							ld_index = old_offset + i - 1;
						} in
						debug_locals <- ld :: debug_locals;
						loop (i - (signature_size t)) l
					| [] ->
						assert false
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
		| _ -> assert false

	(** Emits an invokeinterface instruction to invoke method [name] on [path] with signature [jsigm]. **)
	method invokeinterface (path : jpath) (name : string) (jsigm : jsignature) = match jsigm with
		| TMethod(tl,tr) ->
			let offset = code#get_pool#add_field path name jsigm FKInterfaceMethod in
			code#invokeinterface offset (object_path_sig path) tl (match tr with None -> [] | Some tr -> [tr])
		| _ -> assert false

	(** Emits an invokespecial instruction to invoke method [name] on [path] with signature [jsigm]. **)
	method invokespecial (path : jpath) (name : string) (jsigm : jsignature) = match jsigm with
		| TMethod(tl,tr) ->
			let offset = code#get_pool#add_field path name jsigm FKMethod in
			code#invokespecial offset (object_path_sig path) tl (match tr with None -> [] | Some tr -> [tr])
		| _ -> assert false

	(** Emits an invokestatic instruction to invoke method [name] on [path] with signature [jsigm]. **)
	method invokestatic (path : jpath) (name : string) (jsigm : jsignature) = match jsigm with
		| TMethod(tl,tr) ->
			let offset = code#get_pool#add_field path name jsigm FKMethod in
			code#invokestatic offset tl (match tr with None -> [] | Some tr -> [tr])
		| _ -> assert false

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
	method add_argument_and_field (name : string) (jsig_field : jsignature) =
		assert (not (self#has_method_flag MStatic));
		let jf = new builder jc name jsig_field in
		jf#add_access_flag 1;
		jc#add_field jf#export_field;
		let _,load,_ = self#add_local name jsig_field VarArgument in
		self#load_this;
		load();
		self#putfield jc#get_this_path name jsig_field;

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
			if not no_value then self#set_top_initialized (object_path_sig path);
			if not no_value then code#dup;
			let jsigs = f () in
			self#invokevirtual path "new" (method_sig jsigs None);
		| ConstructInit ->
			if not no_value then code#dup;
			let jsigs = f () in
			self#invokespecial path "<init>" (method_sig jsigs None);
			if not no_value then self#set_top_initialized (object_path_sig path)

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

	(** Adds a closure to method [name] ob [path] with signature [jsig_method] to the constant pool.

	    Also emits an instruction to load the closure.
	**)
	method read_closure is_static path name jsig_method =
		let offset = code#get_pool#add_field path name jsig_method FKMethod in
		let offset = code#get_pool#add (ConstMethodHandle((if is_static then 6 else 5), offset)) in
		code#ldc offset jsig_method

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
			end
		| _ ->
			assert false

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

	method adapt_method jsig =
		()
		(* let offset = code#get_pool#add_string (generate_method_signature false jsig) in
		let offset = code#get_pool#add (ConstMethodType offset) in
		self#get_code#dup;
		self#if_then
			(fun () -> self#get_code#if_null_ref jsig)
			(fun () ->
				code#ldc offset method_type_sig;
				self#invokevirtual method_handle_path "asType" (method_sig [method_type_sig] (Some method_handle_sig))
			);
		ignore(code#get_stack#pop);
		code#get_stack#push jsig; *)

	(** Casts the top of the stack to [jsig]. If [allow_to_string] is true, Jvm.toString is called. **)
	method cast ?(not_null=false) ?(allow_to_string=false) jsig =
		let jsig' = code#get_stack#top in
		begin match jsig,jsig' with
		| TObject((["java";"lang"],"Double"),_),TInt ->
			code#i2d;
			self#expect_reference_type;
		| TObject((["java";"lang"],"Double"),_),TObject((["java";"lang"],"Integer"),_) ->
			self#invokestatic (["haxe";"jvm"],"Jvm") "nullIntToNullFloat" (method_sig [integer_sig] (Some double_sig))
		| TObject((["java";"lang"],"Double"),_),TObject((["java";"lang"],"Object"),_) ->
			self#invokestatic (["haxe";"jvm"],"Jvm") "dynamicToNullFloat" (method_sig [object_sig] (Some double_sig))
		(* from double *)
		| TFloat,TDouble ->
			code#d2f
		| TInt,TDouble ->
			code#d2i;
		| TLong,TDouble ->
			code#d2l;
		(* from float *)
		| TDouble,TFloat ->
			code#f2d
		| TInt,TFloat ->
			code#f2i;
		| TLong,TFloat ->
			code#f2l;
		(* from int *)
		| TBool,TInt ->
			ignore(code#get_stack#pop);
			code#get_stack#push TBool;
		| TByte,TInt ->
			code#i2b TByte
		| TChar,TInt ->
			code#i2c
		| TDouble,TInt ->
			code#i2d;
		| TFloat,TInt ->
			code#i2f
		| TLong,TInt ->
			code#i2l;
		| TShort,TInt ->
			code#i2s
		(* from long *)
		| TDouble,TLong ->
			code#l2d;
		| TFloat,TLong ->
			code#l2f
		| TInt,TLong ->
			code#l2i;
		(* widening *)
		| TInt,(TByte | TShort | TChar) ->
			(* No cast, but rewrite stack top *)
			ignore(code#get_stack#pop);
			code#get_stack#push jsig;
		| TObject(path1,_),TObject(path2,_) when path1 = path2 ->
			()
		| TObject((["java";"lang"],"String"),_),_ when allow_to_string ->
			self#expect_reference_type;
			self#invokestatic (["haxe";"jvm"],"Jvm") "toString" (method_sig [object_sig] (Some string_sig))
		| TObject(path1,_),TObject(path2,_) ->
			if path1 = object_path then begin
				(* We should never need a checkcast to Object, but we should adjust the stack so stack maps are wide enough *)
				ignore(code#get_stack#pop);
				code#get_stack#push object_sig
			end else
				code#checkcast path1;
		| TObject(path,_),TTypeParameter _ ->
			code#checkcast path
		| TMethod _,TMethod _ ->
			if jsig <> jsig' then self#adapt_method jsig;
		| TMethod _,TObject((["java";"lang";"invoke"],"MethodHandle"),_) ->
			self#adapt_method jsig;
		| TObject((["java";"lang";"invoke"],"MethodHandle"),_),TMethod _ ->
			()
		| TMethod _,_ ->
			code#checkcast (["java";"lang";"invoke"],"MethodHandle");
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
		let old_terminated = terminated in
		(fun () ->
			code#get_stack#restore save;
			terminated <- old_terminated;
		)

	(** Generates code which executes [f_if()] and then branches into [f_then()] and [f_else()]. **)
	method if_then_else (f_if : unit -> jbranchoffset ref) (f_then : unit -> unit) (f_else : unit -> unit) =
		let jump_then = f_if () in
		let restore = self#start_branch in
		let pop = self#push_scope in
		f_then();
		pop();
		let r_then = ref code#get_fp in
		let term_then = self#is_terminated in
		if not term_then then code#goto r_then;
		jump_then := code#get_fp - !jump_then;
		restore();
		self#add_stack_frame;
		let pop = self#push_scope in
		f_else();
		pop();
		self#set_terminated (term_then && self#is_terminated);
		r_then := code#get_fp - !r_then;
		if not self#is_terminated then self#add_stack_frame

	(** Generates code which executes [f_if()] and then branches into [f_then()], if the condition holds. **)
	method if_then (f_if : unit -> jbranchoffset ref) (f_then : unit -> unit) =
		let jump_then = f_if () in
		let restore = self#start_branch in
		let pop = self#push_scope in
		f_then();
		pop();
		restore();
		jump_then := code#get_fp - !jump_then;
		self#add_stack_frame

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


	(**
		Emits a tableswitch or lookupswitch instruction, depending on which one makes more sense.

		The switch subject is expected to be on the stack before calling this function.

		If [is_exhaustive] is true and [def] is None, the first case is used as the default case.
	**)
	method int_switch (is_exhaustive : bool) (cases : (Int32.t list * (unit -> unit)) list) (def : (unit -> unit) option) =
		let def,cases = match def,cases with
			| None,(_,ec) :: cases when is_exhaustive ->
				Some ec,cases
			| _ ->
				def,cases
		in
		let flat_cases = DynArray.create () in
		let case_lut = ref Int32Map.empty in
		let fp = code#get_fp in
		let imin = ref Int32.min_int in
		let imax = ref Int32.max_int in
		let cases = List.map (fun (il,f) ->
			let rl = List.map (fun i32 ->
				let r = ref fp in
				if i32 < !imin then imin := i32;
				if i32 > !imax then imax := i32;
				DynArray.add flat_cases (i32,r);
				case_lut := Int32Map.add i32 r !case_lut;
				r
			) il in
			(rl,f)
		) cases in
		let offset_def = ref fp in
		(* No idea what's a good heuristic here... *)
		let diff = Int32.sub !imax !imin in
		let use_tableswitch = diff < (Int32.of_int (DynArray.length flat_cases + 10)) && diff >= Int32.zero (* #8388 *) in
		if use_tableswitch then begin
			let offsets = Array.init (Int32.to_int (Int32.sub !imax !imin) + 1) (fun i ->
				try Int32Map.find (Int32.add (Int32.of_int i) !imin) !case_lut
				with Not_found -> offset_def
			) in
			code#tableswitch offset_def !imin !imax offsets
		end else begin
			let a = DynArray.to_array flat_cases in
			Array.sort (fun (i1,_) (i2,_) -> compare i1 i2) a;
			code#lookupswitch offset_def a;
		end;
		let restore = self#start_branch in
		let offset_exit = ref code#get_fp in
		let def_term,r_def = match def with
			| None ->
				true,ref 0
			| Some f ->
				offset_def := code#get_fp - !offset_def;
				self#add_stack_frame;
				let pop_scope = self#push_scope in
				f();
				pop_scope();
				self#is_terminated,self#maybe_make_jump
		in
		let rec loop acc cases = match cases with
		| (rl,f) :: cases ->
			restore();
			self#add_stack_frame;
			List.iter (fun r -> r := code#get_fp - !r) rl;
			let pop_scope = self#push_scope in
			f();
			pop_scope();
			let r = if cases = [] then ref 0 else self#maybe_make_jump in
			loop ((self#is_terminated,r) :: acc) cases
		| [] ->
			List.rev acc
		in
		let rl = loop [] cases in
		self#close_jumps (def <> None) ((def_term,if def = None then offset_def else r_def) :: rl);
		if def = None then code#get_fp else !offset_exit

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
			| [] -> assert false
		in
		loop locals

	method set_this_initialized =
		let rec loop acc locals = match locals with
			| [(init,name,_)] -> List.rev ((init,name,jc#get_jsig) :: acc)
			| [] -> assert false
			| l :: locals -> loop (l :: acc) locals
		in
		locals <- loop [] locals

	method set_top_initialized jsig =
		ignore(code#get_stack#pop);
		code#get_stack#push jsig

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
	method is_terminated = terminated
	method get_name = name
	method get_jsig = jsig
	method set_terminated b = terminated <- b

	method private get_jcode (config : export_config) =
		let attributes = DynArray.create () in
		let lines = code#get_lines in
		if config.export_debug && DynArray.length lines > 0 then
			DynArray.add attributes (AttributeLineNumberTable (DynArray.to_array lines));
		let stack_map_table = self#get_stack_map_table in
		if Array.length stack_map_table > 0 then
			DynArray.add attributes (AttributeStackMapTable stack_map_table);
		let exceptions = Array.of_list (List.rev exceptions) in
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
		if config.export_debug then begin match debug_locals with
		| [] ->
			()
		| _ ->
			let a = Array.of_list debug_locals in
			self#add_attribute (AttributeLocalVariableTable a);
		end;
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