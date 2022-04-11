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
open Ast
open Type
open Common
open EvalValue
open EvalContext
open EvalPrototype
open EvalExceptions
open EvalJit
open EvalJitContext
open EvalPrinting
open EvalMisc
open EvalHash
open EvalEncode
open EvalField
open MacroApi
open Extlib_leftovers

(* Create *)

let create com api is_macro =
	let t = Timer.timer [(if is_macro then "macro" else "interp");"create"] in
	incr GlobalState.sid;
	let builtins = match !GlobalState.stdlib with
		| None ->
			let builtins = {
				static_builtins = IntMap.empty;
				instance_builtins = IntMap.empty;
				constructor_builtins = Hashtbl.create 0;
				empty_constructor_builtins = Hashtbl.create 0;
			} in
			EvalStdLib.init_standard_library builtins;
			GlobalState.stdlib := Some builtins;
			builtins
		| Some (builtins) ->
			builtins
	in
	let debug = match !GlobalState.debug with
		| None ->
			let support_debugger = Common.defined com Define.EvalDebugger in
			let socket =
				try
					if not support_debugger then raise Exit;
					let fail msg =
						print_endline msg;
						raise Exit
					in
					let s = Common.defined_value com Define.EvalDebugger in
					let host,port = try ExtString.String.split s ":" with _ -> fail "Invalid host format, expected host:port" in
					let port = try int_of_string port with _ -> fail "Invalid port, expected int" in
					Some (try
						let socket = Socket.create host port in
						{
							socket = socket;
							connection = EvalDebugSocket.make_connection socket;
						};
					with exc ->
						fail (Printexc.to_string exc)
					)
				with _ ->
					None
			in
			let debug' = {
				breakpoints = Hashtbl.create 0;
				function_breakpoints = Hashtbl.create 0;
				support_debugger = support_debugger;
				debug_socket = socket;
				exception_mode = CatchUncaught;
				debug_context = new eval_debug_context;
			} in
			GlobalState.debug := Some debug';
			debug'
		| Some debug ->
			debug
	in
	let detail_times = Common.defined com Define.EvalTimes in
	let thread = {
		tthread = Thread.self();
		tstorage = IntMap.empty;
		tevents = vnull;
		tdeque = EvalThread.Deque.create();
	} in
	let eval = EvalThread.create_eval thread in
	let evals = IntMap.singleton 0 eval in
	let rec ctx = {
		ctx_id = !GlobalState.sid;
		is_macro = is_macro;
		debug = debug;
		detail_times = detail_times;
		curapi = api;
		builtins = builtins;
		type_cache = IntMap.empty;
		overrides = Hashtbl.create 0;
		had_error = false;
		(* prototypes *)
		string_prototype = fake_proto key_String;
		array_prototype = fake_proto key_Array;
		vector_prototype = fake_proto key_eval_Vector;
		static_prototypes = new static_prototypes;
		instance_prototypes = IntMap.empty;
		constructors = IntMap.empty;
		file_keys = com.file_keys;
		get_object_prototype = get_object_prototype;
		(* eval *)
		toplevel = 	vobject {
			ofields = [||];
			oproto = OProto (fake_proto key_eval_toplevel);
		};
		eval = eval;
		evals = evals;
		exception_stack = [];
		max_stack_depth = int_of_string (Common.defined_value_safe ~default:"1000" com Define.EvalCallStackDepth);
	} in
	if debug.support_debugger && not !GlobalState.debugger_initialized then begin
		(* Let's wait till the debugger says we're good to continue. This allows it to finish configuration.
		   Note that configuration is shared between macro and interpreter contexts, which is why the check
		   is governed by a global variable. *)
		GlobalState.debugger_initialized := true;
		 (* There's select_ctx in the json-rpc handling, so let's select this one. It's fine because it's the
		    first context anyway. *)
		select ctx;
		ignore(Event.sync(Event.receive eval.debug_channel));
	end;
	(* If no user-defined exception handler is set then follow libuv behavior.
		Which is printing an error to stderr and exiting with code 2 *)
	Luv.Error.set_on_unhandled_exception (fun ex ->
		match ex with
		| Sys_exit _ -> raise ex
		| _ ->
			let msg =
				match ex with
				| Error.Error (err,_) -> Error.error_msg err
				| _ -> Printexc.to_string ex
			in
			Printf.eprintf "%s\n" msg;
			exit 2
	);
	t();
	ctx

(* API for macroContext.ml *)

let call_path ctx path f vl api =
	if ctx.had_error then
		None
	else begin
		let old = ctx.curapi in
		ctx.curapi <- api;
		let path = match List.rev path with
			| [] -> die "" __LOC__
			| name :: path -> List.rev path,name
		in
		catch_exceptions ctx ~final:(fun () -> ctx.curapi <- old) (fun () ->
			let vtype = get_static_prototype_as_value ctx (path_hash path) api.pos in
			let vfield = field vtype (hash f) in
			let p = api.pos in
			let info = create_env_info true p.pfile (ctx.file_keys#get p.pfile) EKEntrypoint (Hashtbl.create 0) 0 0 in
			let env = push_environment ctx info in
			env.env_leave_pmin <- p.pmin;
			env.env_leave_pmax <- p.pmax;
			let v = call_value_on vtype vfield vl in
			pop_environment ctx env;
			v
		) api.pos
	end

let value_signature v =
	let buf = Buffer.create 0 in
	let add s = Buffer.add_string buf s in
	let addc c = Buffer.add_char buf c in
	let scache = Hashtbl.create 0 in
	let adds s =
		try
			let i = Hashtbl.find scache s in
			addc 'R';
			add (string_of_int i)
		with Not_found ->
			Hashtbl.add scache s (Hashtbl.length scache);
			addc 'y';
			let s = EvalStdLib.StdStringTools.url_encode s in
			add (string_of_int (String.length s));
			addc ':';
			add s
	in
	let cache = ValueHashtbl.create 0 in
	let cache_length = ref 0 in
	let cache v f =
		try
			let i = ValueHashtbl.find cache v in
			addc 'r';
			add (string_of_int i)
		with Not_found ->
			let i = !cache_length in
			ValueHashtbl.add cache v i;
			incr cache_length;
			f()
	in
	let custom_count = ref 0 in
	(* Custom format: enumerate custom entities as name_char0, name_char1 etc. *)
	let custom_name name_char =
		cache v (fun () ->
			addc 'F';
			add (string_of_int !custom_count);
			incr custom_count
		)
	in
	let rec loop v = match v with
		| VNull -> addc 'n'
		| VTrue -> addc 't'
		| VFalse -> addc 'f'
		| VInt32 i when i = Int32.zero -> addc 'z'
		| VInt32 i ->
			addc 'i';
			add (Int32.to_string i)
		| VInt64 i ->
			add "i64";
			add (Signed.Int64.to_string i)
		| VUInt64 u ->
			add "u64";
			add (Unsigned.UInt64.to_string u)
		| VFloat f ->
			if f = neg_infinity then addc 'm'
			else if f = infinity then addc 'p'
			else if f <> f then addc 'k'
			else begin
				addc 'd';
				add (string_of_float f)
			end
		| VEnumValue ve ->
			cache v (fun () ->
				addc 'j';
				adds (rev_hash ve.epath);
				addc ':';
				add (string_of_int ve.eindex);
				addc ':';
				add (string_of_int (Array.length ve.eargs));
				Array.iter loop ve.eargs;
			)
		| VObject o ->
			cache v (fun () ->
				addc 'o';
				let fields = object_fields o in
				loop_fields fields;
				addc 'g'
			)
		| VInstance {ikind = IDate f} ->
			cache v (fun () ->
				addc 'v';
				add ((s_date f).sstring)
			)
		| VInstance {ikind = IStringMap map} ->
			cache v (fun() ->
				addc 'b';
				StringHashtbl.iter (fun s (_,value) ->
					adds s;
					loop value
				) map;
				addc 'h'
			)
		| VInstance {ikind = IIntMap map} ->
			cache v (fun () ->
				addc 'q';
				IntHashtbl.iter (fun i value ->
					addc ':';
					add (string_of_int i);
					loop value
				) map;
				addc 'h'
			)
		| VInstance {ikind = IObjectMap map} ->
			cache v (fun() ->
				addc 'M';
				ValueHashtbl.iter (fun key value ->
					loop key;
					loop value
				) (Obj.magic map);
				addc 'h'
			)
		| VInstance {ikind = IBytes b} ->
			cache v (fun () ->
				addc 's';
				let base64_chars = [|
					'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';
					'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';
					'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';
					'w';'x';'y';'z';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'%';':'
				|] in
				let s = Bytes.unsafe_to_string (Base64.str_encode ~tbl:(base64_chars) (Bytes.unsafe_to_string b)) in
				add (string_of_int (String.length s));
				addc ':';
				add s
			)
		| VInstance i ->
			cache v (fun () ->
				addc 'c';
				adds (rev_hash i.iproto.ppath);
				let fields = instance_fields i in
				loop_fields fields;
				addc 'g';
			)
		| VString s ->
			adds s.sstring
		| VNativeString s ->
			add s
		| VArray {avalues = a} | VVector a ->
			cache v (fun () ->
				addc 'a';
				let nulls null_count =
					if null_count > 0 then begin
						addc 'u';
						add (string_of_int null_count);
					end
				in
				let rec loop2 null_count vl = match vl with
					| VNull :: vl -> loop2 (null_count + 1) vl
					| v :: vl ->
						nulls null_count;
						loop v;
						loop2 0 vl
					| [] ->
						nulls null_count
				in
				loop2 0 (Array.to_list a);
				addc 'h'
			)
		| VPrototype {pkind = PClass _; ppath = path} ->
			addc 'A';
			adds (rev_hash path)
		| VPrototype {pkind = PEnum _; ppath = path} ->
			addc 'B';
			adds (rev_hash path)
		| VPrototype _ ->
			die "" __LOC__
		| VFunction _ | VFieldClosure _ ->
			custom_name 'F'
		| VHandle _ ->
			custom_name 'H'
		| VLazy f ->
			loop (!f())
	and loop_fields fields =
		List.iter (fun (name,v) ->
			adds (rev_hash name);
			loop v;
		) fields
	in
	loop v;
	Digest.string (Buffer.contents buf)

let prepare_callback = EvalMisc.prepare_callback

let init ctx = ()

let setup get_api =
	let api = get_api (fun() -> (get_ctx()).curapi.get_com()) (fun() -> (get_ctx()).curapi) in
	List.iter (fun (n,v) -> match v with
		| VFunction(f,b) ->
			let f vl = try
				f vl
			with
			| Sys_error msg | Failure msg | Invalid_argument msg ->
				exc_string msg
			| MacroApi.Invalid_expr ->
				exc_string "Invalid expression"
			in
			let v = VFunction (f,b) in
			Hashtbl.replace GlobalState.macro_lib n v
		| _ -> die "" __LOC__
	) api;
	Globals.macro_platform := Globals.Eval

let do_reuse ctx api =
	ctx.curapi <- api;
	ctx.static_prototypes#reset

let set_error ctx b =
	(* TODO: Have to reset this somewhere if running compilation server. But where... *)
	ctx.had_error <- b

let add_types ctx types ready =
	if not ctx.had_error then ignore(catch_exceptions ctx (fun () -> ignore(add_types ctx types ready)) null_pos)

let compiler_error msg pos =
	let vi = encode_instance key_haxe_macro_Error in
	match vi with
	| VInstance i ->
		let msg = EvalString.create_unknown msg in
		set_instance_field i key_exception_message msg;
		set_instance_field i key_pos (encode_pos pos);
		set_instance_field i key_native_exception msg;
		let ctx = get_ctx() in
		let eval = get_eval ctx in
		(match eval.env with
		| Some _ ->
			let stack = EvalStackTrace.make_stack_value (call_stack eval) in
			set_instance_field i key_native_stack stack;
		| None -> ());
		exc vi
	| _ ->
		die "" __LOC__

let rec value_to_expr v p =
	let path i =
		let mt = IntMap.find i (get_ctx()).type_cache in
		let make_path t =
			let rec loop = function
				| [] -> die "" __LOC__
				| [name] -> (EConst (Ident name),p)
				| name :: l -> (efield (loop l,name),p)
			in
			let t = t_infos t in
			loop (List.rev (if t.mt_module.m_path = t.mt_path then fst t.mt_path @ [snd t.mt_path] else fst t.mt_module.m_path @ [snd t.mt_module.m_path;snd t.mt_path]))
		in
		make_path mt
	in
	let make_map_entry e_key v =
		let e_value = value_to_expr v p in
		(EBinop(OpArrow,e_key,e_value),p)
	in
	match vresolve v with
	| VNull -> (EConst (Ident "null"),p)
	| VTrue -> (EConst (Ident "true"),p)
	| VFalse -> (EConst (Ident "false"),p)
	| VInt32 i -> (EConst (Int (Int32.to_string i,None)),p)
	| VFloat f -> haxe_float f p
	| VString s -> (EConst (String(s.sstring,SDoubleQuotes)),p)
	| VArray va -> (EArrayDecl (List.map (fun v -> value_to_expr v p) (EvalArray.to_list va)),p)
	| VObject o -> (EObjectDecl (ExtList.List.filter_map (fun (k,v) ->
			let n = rev_hash k in
			(* Workaround for #8261: Ignore generated pos fields *)
			begin match v with
			| VInstance {ikind = IPos _} when n = "pos" -> None
			| _ -> Some ((n,p,(if Lexer.is_valid_identifier n then NoQuotes else DoubleQuotes)),(value_to_expr v p))
			end
		) (object_fields o)),p)
	| VEnumValue e ->
		let epath =
			let proto = get_static_prototype_raise (get_ctx()) e.epath in
			let expr = path e.epath in
			let name = match proto.pkind with
				| PEnum names -> fst (List.nth names e.eindex)
				| _ -> die "" __LOC__
			in
			(efield (expr, name), p)
		in
		begin
			match e.eargs with
			| [||] -> epath
			| _ ->
				let args = List.map (fun v -> value_to_expr v p) (Array.to_list e.eargs) in
				(ECall (epath, args), p)
		end
	| VInstance {ikind = IIntMap m} ->
		let el = IntHashtbl.fold (fun k v acc ->
			let e_key = (EConst (Int (string_of_int k, None)),p) in
			(make_map_entry e_key v) :: acc
		) m [] in
		(EArrayDecl el,p)
	| VInstance {ikind = IStringMap m} ->
		let el = StringHashtbl.fold (fun k (_,v) acc ->
			let e_key = (EConst (String(k,SDoubleQuotes)),p) in
			(make_map_entry e_key v) :: acc
		) m [] in
		(EArrayDecl el,p)
	| VInstance {ikind = IObjectMap m} ->
		let el = Hashtbl.fold (fun k v acc ->
			let e_key = value_to_expr k p in
			(make_map_entry e_key v) :: acc
		) m [] in
		(EArrayDecl el,p)
	| _ -> exc_string ("Cannot convert " ^ (value_string v) ^ " to expr")

let encode_obj = encode_obj_s

let field v f = field v (EvalHash.hash f)

let value_string = value_string

let exc_string = exc_string

let eval_expr ctx e = if ctx.had_error then None else eval_expr ctx EKEntrypoint e

let handle_decoding_error f v t =
	let line = ref 1 in
	let errors = ref [] in
	let error msg v =
		errors := (msg,!line) :: !errors;
		f (Printf.sprintf "%s <- %s" (value_string v) msg)
	in
	let rec loop tabs t v =
		match t with
		| TAnon an ->
			f "{";
			PMap.iter (fun _ cf ->
				incr line;
				f (Printf.sprintf "\n%s%s: " (tabs ^ "\t") cf.cf_name);
				try
					let vf = field_raise v (EvalHash.hash cf.cf_name) in
					begin match vf with
					| VNull when not (is_explicit_null cf.cf_type) -> error "expected value" vf
					| _ -> loop (tabs ^ "\t") cf.cf_type vf
					end
				with Not_found ->
					if not (is_explicit_null cf.cf_type) then error "expected value" VNull
					else f "null"
			) an.a_fields;
			incr line;
			f (Printf.sprintf "\n%s}" tabs)
		| TInst({cl_path=[],"Array"},[t1]) ->
			begin match v with
				| VArray va ->
					f "[";
					let _ = List.fold_left (fun first v ->
						if not first then f ", ";
						loop tabs t1 v;
						false
					) true (EvalArray.to_list va) in
					f "]"
				| _ -> error "expected Array" v
			end
		| TInst({cl_path=[],"String"},_) ->
			begin match v with
				| VString _ -> f (value_string v)
				| _ -> error "expected String" v
			end
		| TAbstract({a_path=[],"Null"},[t1]) ->
			if v = VNull then f "null" else loop tabs t1 v
		| TAbstract({a_path=[],"Bool"},_) ->
			begin match v with
				| VTrue -> f "true"
				| VFalse -> f "false"
				| _ -> error "expected Bool" v
			end
		| TAbstract({a_path=[],("Int" | "Float")},_) ->
			begin match v with
				| VInt32 _ | VFloat _ -> f (value_string v)
				| _ -> error "expected Bool" v
			end
		| TType(t,tl) ->
			loop tabs (apply_typedef t tl) v
		| TAbstract({a_path=["haxe";"macro"],"Position"},_) ->
			begin match v with
				| VInstance {ikind=IPos _} -> f "#pos"
				| _ -> error "expected Position" v
			end
		| TEnum(en,_) ->
			begin match v with
				| VEnumValue ev ->
					let ef = PMap.find (List.nth en.e_names ev.eindex) en.e_constrs in
					f ef.ef_name;
					let rec loop2 first tl vl = match tl,vl with
						| _,[] -> ()
						| [],_ -> ()
						| (_,_,t) :: tl,v :: vl ->
							if not first then f ", ";
							loop tabs t v;
							loop2 false tl vl
					in
					begin match follow ef.ef_type,Array.to_list ev.eargs with
						| _,[] ->
							()
						| TFun(tl,_),vl ->
							 f "(";
							loop2 true tl vl;
							f ")"
						| _ -> ()
					end
				| _ -> error "expected enum value" v
			end
		| TInst _ | TAbstract _ | TFun _ ->
			(* TODO: might need some more of these, not sure *)
			die "" __LOC__
		| TMono r ->
			begin match r.tm_type with
				| None -> ()
				| Some t -> loop tabs t v
			end
		| TLazy r ->
			loop tabs (lazy_type r) v
		| TDynamic _ ->
			()
	in
	loop "" t v;
	!errors

let get_api_call_pos () =
	let eval = get_eval (get_ctx()) in
	let env = Option.get eval.env in
	let env = match env.env_parent with
		| None -> env
		| Some env -> env
	in
	{ pfile = rev_hash env.env_info.pfile; pmin = env.env_leave_pmin; pmax = env.env_leave_pmax }
