(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

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

(* Create *)

let sid = ref (-1)

let stdlib = ref None
let debug = ref None

let create com api is_macro =
	let t = Timer.timer [(if is_macro then "macro" else "interp");"create"] in
	incr sid;
	let builtins = match !stdlib with
		| None ->
			let builtins = {
				static_builtins = IntMap.empty;
				instance_builtins = IntMap.empty;
				constructor_builtins = Hashtbl.create 0;
				empty_constructor_builtins = Hashtbl.create 0;
			} in
			EvalStdLib.init_standard_library builtins;
			stdlib := Some builtins;
			builtins
		| Some (builtins) ->
			builtins
	in
	let debug = match !debug with
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
					if s = "1" then raise Exit;
					let host,port = try ExtString.String.split s ":" with _ -> fail "Invalid host format, expected host:port" in
					let port = try int_of_string port with _ -> fail "Invalid port, expected int" in
					Some (try Socket.create host port with exc -> fail (Printexc.to_string exc))
				with _ ->
					None
			in
			let debug' = {
				debug = com.Common.debug || support_debugger;
				breakpoints = Hashtbl.create 0;
				support_debugger = support_debugger;
				debug_state = DbgStart;
				breakpoint = EvalDebugMisc.make_breakpoint 0 0 BPDisabled BPAny None;
				caught_types = Hashtbl.create 0;
				environment_offset_delta = 0;
				debug_socket = socket;
				exception_mode = CatchUncaught;
			} in
			debug := Some debug';
			debug'
		| Some debug ->
			debug
	in
	let detail_times = Common.defined com Define.EvalTimes in
	let evals = DynArray.create () in
	let eval = {
		environments = DynArray.make 32;
		environment_offset = 0;
	} in
	DynArray.add evals eval;
	let rec ctx = {
		ctx_id = !sid;
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
		static_prototypes = IntMap.empty;
		instance_prototypes = IntMap.empty;
		constructors = IntMap.empty;
		get_object_prototype = get_object_prototype;
		(* eval *)
		toplevel = 	vobject {
			ofields = [||];
			oproto = fake_proto key_eval_toplevel;
		};
		eval = eval;
		exception_stack = [];
	} in
	t();
	ctx

(* API for macroContext.ml *)

let eval_delayed ctx e =
	let jit,f = jit_expr ctx e in
	let info = create_env_info true (file_hash e.epos.pfile) EKDelayed jit.capture_infos in
	fun () ->
		let env = push_environment ctx info jit.max_num_locals (Hashtbl.length jit.captures) in
		match catch_exceptions ctx (fun () -> Std.finally (fun _ -> pop_environment ctx env) f env) e.epos with
			| Some v -> v
			| None -> vnull

let call_path ctx path f vl api =
	if ctx.had_error then
		None
	else begin
		let old = ctx.curapi in
		ctx.curapi <- api;
		let path = match List.rev path with
			| [] -> assert false
			| name :: path -> List.rev path,name
		in
		catch_exceptions ctx ~final:(fun () -> ctx.curapi <- old) (fun () ->
			let vtype = get_static_prototype_as_value ctx (path_hash path) api.pos in
			let vfield = field vtype (hash_s f) in
			call_value_on vtype vfield vl
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
			add (string_of_int (Rope.length s));
			addc ':';
			add (Rope.to_string s)
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
	let function_count = ref 0 in
	let rec loop v = match v with
		| VNull -> addc 'n'
		| VTrue -> addc 't'
		| VFalse -> addc 'f'
		| VInt32 i when i = Int32.zero -> addc 'z'
		| VInt32 i ->
			addc 'i';
			add (Int32.to_string i)
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
				adds (rev_hash_s ve.epath);
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
				add (EvalString.get (s_date f))
			)
		| VInstance {ikind = IStringMap map} ->
			cache v (fun() ->
				addc 'b';
				StringHashtbl.iter (fun s value ->
					adds (Lazy.force s.sstring);
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
				adds (rev_hash_s i.iproto.ppath);
				let fields = instance_fields i in
				loop_fields fields;
				addc 'g';
			)
		| VString s ->
			adds (Lazy.force s.sstring)
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
			adds (rev_hash_s path)
		| VPrototype {pkind = PEnum _; ppath = path} ->
			addc 'B';
			adds (rev_hash_s path)
		| VPrototype _ ->
			assert false
		| VFunction _ | VFieldClosure _ ->
			(* Custom format: enumerate functions as F0, F1 etc. *)
			cache v (fun () ->
				addc 'F';
				add (string_of_int !function_count);
				incr function_count
			)
		| VLazy f ->
			loop (!f())
	and loop_fields fields =
		List.iter (fun (name,v) ->
			adds (rev_hash_s name);
			loop v;
		) fields
	in
	loop v;
	Digest.string (Buffer.contents buf)

let prepare_callback v n =
	match v with
	| VFunction _ | VFieldClosure _ ->
		let ctx = get_ctx() in
		(fun args -> match catch_exceptions ctx (fun() -> call_value v args) null_pos with
			| Some v -> v
			| None -> vnull)
	| _ ->
		raise Invalid_expr

let init ctx = ()

let setup get_api =
	let api = get_api (fun() -> (get_ctx()).curapi.get_com()) (fun() -> (get_ctx()).curapi) in
	List.iter (fun (n,v) -> match v with
		| VFunction(f,b) ->
			let f vl = try
				f vl
			with
			| Sys_error msg | Failure msg ->
				exc_string msg
			| MacroApi.Invalid_expr ->
				exc_string "Invalid expression"
			in
			let v = VFunction (f,b) in
			Hashtbl.replace EvalStdLib.macro_lib n v
		| _ -> assert false
	) api;
	Globals.macro_platform := Globals.Eval

let can_reuse ctx types = true

let do_reuse ctx api =
	ctx.curapi <- api

let set_error ctx b =
	(* TODO: Have to reset this somewhere if running compilation server. But where... *)
	ctx.had_error <- b

let add_types ctx types ready =
	ignore(catch_exceptions ctx (fun () -> ignore(add_types ctx types ready)) null_pos)

let compiler_error msg pos =
	let vi = encode_instance key_haxe_macro_Error in
	match vi with
	| VInstance i ->
		set_instance_field i key_message (EvalString.create_unknown msg);
		set_instance_field i key_pos (encode_pos pos);
		exc vi
	| _ ->
		assert false

let rec value_to_expr v p =
	let path i =
		let mt = IntMap.find i (get_ctx()).type_cache in
		let make_path t =
			let rec loop = function
				| [] -> assert false
				| [name] -> (EConst (Ident name),p)
				| name :: l -> (EField (loop l,name),p)
			in
			let t = t_infos t in
			loop (List.rev (if t.mt_module.m_path = t.mt_path then fst t.mt_path @ [snd t.mt_path] else fst t.mt_module.m_path @ [snd t.mt_module.m_path;snd t.mt_path]))
		in
		make_path mt
	in
	match vresolve v with
	| VNull -> (EConst (Ident "null"),p)
	| VTrue -> (EConst (Ident "true"),p)
	| VFalse -> (EConst (Ident "false"),p)
	| VInt32 i -> (EConst (Int (Int32.to_string i)),p)
	| VFloat f -> haxe_float f p
	| VString s -> (EConst (String (EvalString.get s)),p)
	| VArray va -> (EArrayDecl (List.map (fun v -> value_to_expr v p) (EvalArray.to_list va)),p)
	| VObject o -> (EObjectDecl (List.map (fun (k,v) ->
			let n = rev_hash_s k in
			((n,p,(if Lexer.is_valid_identifier n then NoQuotes else DoubleQuotes)),(value_to_expr v p))
		) (object_fields o)),p)
	| VEnumValue e ->
		let epath =
			let proto = get_static_prototype_raise (get_ctx()) e.epath in
			let expr = path e.epath in
			let name = match proto.pkind with
				| PEnum names -> List.nth names e.eindex
				| _ -> assert false
			in
			(EField (expr, name), p)
		in
		begin
			match e.eargs with
			| [||] -> epath
			| _ ->
				let args = List.map (fun v -> value_to_expr v p) (Array.to_list e.eargs) in
				(ECall (epath, args), p)
		end

	| _ -> exc_string ("Cannot convert " ^ (value_string v) ^ " to expr")

let encode_obj = encode_obj_s

let field v f = field v (EvalHash.hash_s f)

let value_string = value_string

let exc_string = exc_string

let eval_expr ctx e = eval_expr ctx key_questionmark key_questionmark e

let handle_decoding_error v t =
	let line = ref 1 in
	let errors = ref [] in
	let error msg v s =
		errors := (msg,!line) :: !errors;
		Printf.sprintf "%s%s <- %s" s (value_string v) msg
	in
	let rec loop tabs s t v =
		match t with
		| TAnon an ->
			let s = s ^ "{" in
			let s = PMap.fold (fun cf s ->
				incr line;
				let s = Printf.sprintf "%s\n%s%s: " s (tabs ^ "\t") cf.cf_name in
				try
					let vf = field_raise v (EvalHash.hash_s cf.cf_name) in
					begin match vf with
					| VNull when not (is_explicit_null cf.cf_type) -> error "expected value" vf s
					| _ -> loop (tabs ^ "\t") s cf.cf_type vf
					end
				with Not_found ->
					if not (is_explicit_null cf.cf_type) then error "expected value" VNull s
					else s ^ "null"
			) an.a_fields s in
			incr line;
			Printf.sprintf "%s\n%s}" s tabs
		| TInst({cl_path=[],"Array"},[t1]) ->
			begin match v with
				| VArray va ->
					let s = s ^ "[" in
					let s = snd (List.fold_left (fun (first,s) v ->
						let s = if first then s else s ^ ", " in
						false,loop tabs s t1 v
					) (true,s) (EvalArray.to_list va)) in
					s ^ "]"
				| _ -> error "expected Array" v s
			end
		| TInst({cl_path=[],"String"},_) ->
			begin match v with
				| VString _ -> s ^ (value_string v)
				| _ -> error "expected String" v s
			end
		| TAbstract({a_path=[],"Null"},[t1]) ->
			if v = VNull then s ^ "null" else loop tabs s t1 v
		| TAbstract({a_path=[],"Bool"},_) ->
			begin match v with
				| VTrue -> s ^ "true"
				| VFalse -> s ^ "false"
				| _ -> error "expected Bool" v s
			end
		| TAbstract({a_path=[],("Int" | "Float")},_) ->
			begin match v with
				| VInt32 _ | VFloat _ -> s ^ (value_string v)
				| _ -> error "expected Bool" v s
			end
		| TType(t,tl) ->
			loop tabs s (apply_params t.t_params tl t.t_type) v
		| TAbstract({a_path=["haxe";"macro"],"Position"},_) ->
			begin match v with
				| VInstance {ikind=IPos _} -> s ^ "#pos"
				| _ -> error "expected Position" v s
			end
		| TEnum(en,_) ->
			begin match v with
				| VEnumValue ev ->
					let ef = PMap.find (List.nth en.e_names ev.eindex) en.e_constrs in
					let s = Printf.sprintf "%s%s" s ef.ef_name in
					let rec loop2 first s tl vl = match tl,vl with
						| _,[] -> s
						| [],_ -> s (* ? *)
						| (_,_,t) :: tl,v :: vl ->
							let s = if first then s else s ^ ", " in
							let s = loop tabs s t v in
							loop2 false s tl vl
					in
					begin match follow ef.ef_type,Array.to_list ev.eargs with
						| _,[] -> s
						| TFun(tl,_),vl ->
							let s = s ^ "(" in
							let s =  loop2 true s tl vl in
							s ^ ")"
						| _ -> s
					end
				| _ -> error "expected enum value" v s
			end
		| TInst _ | TAbstract _ | TFun _ ->
			(* TODO: might need some more of these, not sure *)
			assert false
		| TMono r ->
			begin match !r with
				| None -> s
				| Some t -> loop tabs s t v
			end
		| TLazy r ->
			loop tabs s (lazy_type r) v
		| TDynamic _ ->
			s (* Nothing we can do *)
	in
	loop "" "" t v,!errors