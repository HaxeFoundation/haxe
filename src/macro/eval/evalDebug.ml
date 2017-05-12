open Gc
open Globals
open Ast
open Type
open EvalJitContext
open EvalContext
open EvalValue
open EvalExceptions
open EvalPrinting
open EvalHash
open EvalEncode
open EvalMisc
open MacroApi

(* Breakpoints *)

let make_breakpoint =
	let id = ref (-1) in
	(fun file line state column ->
		incr id;
		{
			bpid = !id;
			bpfile = file;
			bpline = line;
			bpstate = state;
			bpcolumn = column;
		}
	)

let add_breakpoint ctx file line column =
	let hash = hash_s (Path.unique_full_path (Common.find_file (ctx.curapi.get_com()) file)) in
	let h = try
		Hashtbl.find ctx.debug.breakpoints hash
	with Not_found ->
		let h = Hashtbl.create 0 in
		Hashtbl.add ctx.debug.breakpoints hash h;
		h
	in
	let breakpoint = make_breakpoint hash line BPEnabled column in
	Hashtbl.replace h line breakpoint;
	breakpoint

let delete_breakpoint ctx file line =
	let hash = hash_s (Path.unique_full_path (Common.find_file (ctx.curapi.get_com()) file)) in
	let h = Hashtbl.find ctx.debug.breakpoints hash in
	Hashtbl.remove h line

let iter_breakpoints ctx f =
	Hashtbl.iter (fun _ breakpoints ->
		Hashtbl.iter (fun _ breakpoint -> f breakpoint) breakpoints
	) ctx.debug.breakpoints

let find_breakpoint ctx sid =
	let found = ref None in
	let id = try int_of_string sid with _ -> raise Not_found in
	try
		iter_breakpoints ctx (fun breakpoint ->
			if breakpoint.bpid = id then begin
				found := Some breakpoint;
				raise Exit
			end
		);
		raise Not_found
	with Exit ->
		match !found with None -> assert false | Some breakpoint -> breakpoint

let parse_breakpoint_pattern pattern =
	(* TODO: more than file:line patterns? *)
	try
		let split = ExtString.String.nsplit pattern ":" in
		let file,line,column = match List.rev split with
			| first :: rest ->
				let first = int_of_string first in
				begin match rest with
					| second :: file ->
						begin try
							file,(int_of_string second),BPColumn first
						with _ ->
							(second :: file),first,BPAny
						end
					| file ->
						file,first,BPAny
				end
			| [] -> raise Exit
		in
		let file = String.concat ":" (List.rev file) in
		file,line,column
	with _ ->
		raise Exit

(* Printing *)

let value_string value =
	let rec fields_string depth fields =
		let tabs = String.make (depth * 2) ' ' in
		let l = List.map (fun (name,value) ->
			let s_type,s_value = value_string depth value in
			Printf.sprintf "%s%s : %s = %s" tabs (rev_hash_s name) s_type s_value
		) fields in
		Printf.sprintf "{\n%s\n%s}" (String.concat "\n" l) tabs
	and instance_fields depth vi =
		let fields = IntMap.fold (fun name key acc ->
			(name,vi.ifields.(key)) :: acc
		) vi.iproto.pinstance_names [] in
		fields_string (depth + 1) fields
	and value_string depth v = match v with
		| VNull -> "NULL","null"
		| VTrue -> "Bool","true"
		| VFalse -> "Bool","false"
		| VInt32 i -> "Int",Int32.to_string i
		| VFloat f -> "Float",string_of_float f
		| VEnumValue ev -> rev_hash_s ev.epath,Rope.to_string (s_enum_value 0 ev)
		| VObject o -> "Anonymous",fields_string (depth + 1) (object_fields o)
		| VString(_,s) -> "String","\"" ^ (Ast.s_escape (Lazy.force s)) ^ "\""
		| VArray va -> "Array",Rope.to_string (s_array (depth + 1) va)
		| VInstance vi -> rev_hash_s vi.iproto.ppath,instance_fields (depth + 1) vi
		| VPrototype proto -> "Anonymous",Rope.to_string (s_proto_kind proto)
		| VFunction _ | VFieldClosure _ -> "Function","fun"
	in
	let s_type,s_value = value_string 0 value in
	Printf.sprintf "%s = %s" s_type s_value

(* Vars *)

let get_var_slot_by_name scopes name =
	let rec loop scopes = match scopes with
		| scope :: scopes ->
			begin try
				let id = Hashtbl.find scope.local_ids name in
				let slot = Hashtbl.find scope.locals id in
				slot + scope.local_offset
			with Not_found ->
				loop scopes
			end
		| [] ->
			raise Not_found
	in
	loop scopes

let get_capture_slot_by_name capture_infos name =
	let ret = ref None in
	try
		Hashtbl.iter (fun slot name' ->
			if name = name' then begin
				ret := (Some slot);
				raise Exit
			end
		) capture_infos;
		raise Not_found
	with Exit ->
		match !ret with None -> assert false | Some name -> name

let get_variable capture_infos scopes name env =
	try
		let slot = get_var_slot_by_name scopes name in
		let value = env.env_locals.(slot) in
		value
	with Not_found ->
		let slot = get_capture_slot_by_name capture_infos name in
		let value = try env.env_captures.(slot) with _ -> raise Not_found in
		!value

(* Expr to value *)

let resolve_ident ctx env s =
	let key = hash_s s in
	try
		(* 1. Variable *)
		get_variable env.env_info.capture_infos env.env_debug.scopes s env
	with Not_found -> try
		(* 2. Instance *)
		if env.env_info.static then raise Not_found;
		let v = env.env_locals.(0) in
		EvalField.field_raise v key
	with Not_found -> try
		(* 3. Static *)
		begin match env.env_info.kind with
			| EKMethod(i1,_) ->
				let proto = get_static_prototype_raise ctx i1 in
				EvalField.proto_field_raise proto key
			| _ ->
				raise Not_found
		end
	with Not_found -> try
		(* 4. Type *)
		VPrototype (IntMap.find key ctx.static_prototypes)
	with Not_found ->
		raise Exit

let expr_to_value ctx env e =
	let rec loop e = match fst e with
		| EConst cst ->
			begin match cst with
				| String s -> "",encode_string s
				| Int s -> "",VInt32 (Int32.of_string s)
				| Float s -> "",VFloat (float_of_string s)
				| Ident "true" -> "",VTrue
				| Ident "false" -> "",VFalse
				| Ident "null" -> "",VNull
				| Ident s ->
					let value = resolve_ident ctx env s in
					s,value
				| _ -> raise Exit
			end
		| EArray(e1,eidx) ->
			let n1,v1 = loop e1 in
			let nidx,vidx = loop eidx in
			let idx = match vidx with VInt32 i -> Int32.to_int i | _ -> raise Exit in
			let n = Printf.sprintf "%s[%d]" n1 idx in
			begin match v1 with
				| VArray va ->
					let v = EvalArray.get va idx in
					(n,v)
				| VEnumValue ev ->
					let v = Array.get ev.eargs idx in
					(n,v)
				| _ ->
					raise Exit
			end
		| EField(e1,s) ->
			let n1,v1 = loop e1 in
			let v = EvalField.field v1 (hash_s s) in
			(Printf.sprintf "%s.%s" n1 s),v
		| _ ->
			raise Exit
	in
	loop e

(* Helper *)

let is_caught ctx v =
	try
		Hashtbl.iter (fun path _ -> if is v path then raise Exit) ctx.debug.caught_types;
		false
	with Exit ->
		true

let get_call_stack_envs ctx kind p =
	let envs = match call_stack ctx with
		| _ :: envs -> envs
		| [] -> []
	in
	let rec loop delta envs = match envs with
		| _ :: envs when delta < 0 -> loop (delta + 1) envs
		| _ -> envs
	in
	loop ctx.debug.environment_offset_delta envs

module DebugOutput = struct
	open Unix

	let send_string ctx s = match ctx.debug.debug_socket with
		| None ->
			print_endline s
		| Some socket ->
			begin match socket.socket with
				| None ->
					(* TODO: reconnect? *)
					print_endline s
				| Some socket ->
					let l = String.length s in
					assert (l < 0xFFFF);
					let buf = Bytes.make 2 ' ' in
					Bytes.set buf 0 (Char.unsafe_chr l);
					Bytes.set buf 1 (Char.unsafe_chr (l lsr 8));
					ignore(send socket buf 0 2 []);
					ignore(send socket (Bytes.unsafe_of_string s) 0 (String.length s) [])
			end

	let output_info ctx = send_string ctx
	let output_error ctx = send_string ctx

	let output_exception_stop ctx v pos =
		output_info ctx (uncaught_exception_string v pos "")

	let output_variable_name ctx name =
		send_string ctx (Printf.sprintf "%s" name)

	let output_value ctx name value =
		send_string ctx (Printf.sprintf "%s : %s" name (value_string value))

	let output_call_stack_position ctx i kind p =
		let line = Lexer.get_error_line p in
		send_string ctx (Printf.sprintf "%6i : %s at %s:%i" i (kind_name (get_eval ctx) kind) (Path.get_real_path p.pfile) line)

	let output_call_stack ctx kind p =
		let envs = get_call_stack_envs ctx kind p in
		let i = ref ((get_eval ctx).environment_offset - 1) in
		output_call_stack_position ctx !i kind {p with pfile = Path.unique_full_path p.Globals.pfile};
		List.iter (fun env ->
			if env.env_leave_pmin >= 0 then begin
				let p = {pmin = env.env_leave_pmin; pmax = env.env_leave_pmax; pfile = rev_hash_s env.env_info.pfile} in
				decr i;
				output_call_stack_position ctx !i env.env_info.kind p
			end
		) envs

	let output_file_path ctx s = send_string ctx (Path.get_real_path s)

	let output_type_name ctx = send_string ctx

	let output_breakpoint ctx breakpoint =
		let flag = match breakpoint.bpstate with
			| BPHit | BPEnabled -> "E"
			| BPDisabled -> "d"
		in
		send_string ctx (Printf.sprintf "%i %s" breakpoint.bpid flag)

	let output_breakpoints ctx =
		iter_breakpoints ctx (fun breakpoint ->
			output_breakpoint ctx breakpoint
		)

	let output_breakpoint_set ctx breakpoint =
		output_info ctx (Printf.sprintf "Breakpoint %i set and enabled" breakpoint.bpid)

	let output_breakpoint_stop ctx env =
		output_info ctx (Printf.sprintf "Thread %i stopped in %s at %s:%i." 0 (kind_name (get_eval ctx) env.env_info.kind) (rev_hash_s env.env_info.pfile) env.env_debug.line)

	let output_breakpoint_description ctx breakpoint =
		let s_col = match breakpoint.bpcolumn with
			| BPAny -> ""
			| BPColumn i -> ":" ^ (string_of_int i)
		in
		send_string ctx (Printf.sprintf "%s:%i%s" ((Path.get_real_path (rev_hash_s breakpoint.bpfile))) breakpoint.bpline s_col)

end

module DebugOutputJson = struct
	open Json

	let print_json ctx json =
		let b = Buffer.create 0 in
		write_json (Buffer.add_string b) json;
		DebugOutput.send_string ctx (Buffer.contents b)

	let output_result ctx value =
		print_json ctx (JObject ["result",value])

	let output_success ctx =
		output_result ctx (JString "ok")

	let output_error ctx msg =
		print_json ctx (JObject ["error",JString msg])

	let output_event ctx event data =
		let fields = ["event",JString event] in
		let fields = Option.map_default (fun data -> fields @ ["result",data]) fields data in
		print_json ctx (JObject fields)

	let output_info ctx msg =
		output_result ctx (JString msg)

	let var_to_json name value access =
		let jv t v structured =
			JObject ["name",JString name;"type",JString t;"value",JString v;"structured",JBool structured;"access",JString access]
		in
		let string_repr s = "\"" ^ (Ast.s_escape (Lazy.force s)) ^ "\"" in
		let level2_value_repr = function
			| VNull -> "null"
			| VTrue -> "true"
			| VFalse -> "false"
			| VInt32 i -> Int32.to_string i
			| VFloat f -> string_of_float f
			| VEnumValue ve ->
				let name = EvalPrinting.s_enum_ctor_name ve in
				begin match ve.eargs with
					| [||] -> name
					| vl -> name ^ "(...)"
				end
			| VObject o -> "{...}"
			| VString(_,s) -> string_repr s
			| VArray va -> "[...]"
			| VInstance vi -> (rev_hash_s vi.iproto.ppath) ^ " {...}"
			| VPrototype proto -> Rope.to_string (s_proto_kind proto)
			| VFunction _ | VFieldClosure _ -> "<fun>"
		in
		let fields_string fields =
			let l = List.map (fun (name, value) -> Printf.sprintf "%s: %s" (rev_hash_s name) (level2_value_repr value)) fields in
			Printf.sprintf "{%s}" (String.concat ", " l)
		in
		let array_elems va =
			let l = EvalArray.to_list va in
			let l = List.map level2_value_repr l in
			Printf.sprintf "[%s]" (String.concat ", " l)
		in
		let value_string v = match v with
			| VNull -> jv "NULL" "null" false
			| VTrue -> jv "Bool" "true" false
			| VFalse -> jv "Bool" "false" false
			| VInt32 i -> jv "Int" (Int32.to_string i) false
			| VFloat f -> jv "Float" (string_of_float f) false
			| VEnumValue ve ->
				let type_s = rev_hash_s ve.epath in
				let name = EvalPrinting.s_enum_ctor_name ve in
				let value_s,is_structured = match ve.eargs with
					| [||] -> name, false
					| vl ->
						let l = Array.to_list (Array.map level2_value_repr vl) in
						let s = Printf.sprintf "%s(%s)" name (String.concat ", " l) in
						s, true
				in
				jv type_s value_s is_structured
			| VObject o -> jv "Anonymous" (fields_string (object_fields o)) true (* TODO: false for empty structures *)
			| VString(_,s) -> jv "String" (string_repr s) false
			| VArray va -> jv "Array" (array_elems va) true (* TODO: false for empty arrays *)
			| VInstance vi ->
				let class_name = rev_hash_s vi.iproto.ppath in
				jv class_name (class_name ^ " " ^ (fields_string (instance_fields vi))) true
			| VPrototype proto -> jv "Anonymous" (Rope.to_string (s_proto_kind proto)) false (* TODO: show statics *)
			| VFunction _ | VFieldClosure _ -> jv "Function" "<fun>" false
		in
		value_string value

	let output_variable_name ctx name =
		output_info ctx name

	let output_value ctx name value =
		let o = JObject [
			"name",JString name;
			"value",JString (value_string value)
		] in
		output_result ctx o

	let output_call_stack ctx kind p =
		let envs = get_call_stack_envs ctx kind p in
		let id = ref (-1) in
		let stack_item kind p artificial =
			incr id;
			let line1,col1,line2,col2 = Lexer.get_pos_coords p in
			JObject [
				"id",JInt !id;
				"name",JString (kind_name (get_eval ctx) kind);
				"source",JString (Path.get_real_path p.pfile);
				"line",JInt line1;
				"column",JInt col1;
				"endLine",JInt line2;
				"endColumn",JInt col2;
				"artificial",JBool artificial;
			]
		in
		let l = [stack_item kind p false] in
		let stack = List.fold_left (fun acc env ->
			let p = {pmin = env.env_leave_pmin; pmax = env.env_leave_pmax; pfile = rev_hash_s env.env_info.pfile} in
			(stack_item env.env_info.kind p (env.env_leave_pmin < 0)) :: acc
		) l envs in
		output_result ctx (JArray (List.rev stack))

	let output_file_path ctx s =
		output_info ctx (Path.get_real_path s)

	let output_type_name ctx name =
		output_info ctx name

	let get_breakpoint_description ctx breakpoint =
		let state = function
			| BPEnabled -> "enabled"
			| BPDisabled -> "disabled"
			| BPHit -> "hit"
		in
		let l = [
			"id",JInt breakpoint.bpid;
			"verified",JBool true;
			"source",JString (Path.get_real_path (rev_hash_s breakpoint.bpfile));
			"line",JInt breakpoint.bpline;
			"message",JString (state breakpoint.bpstate)
		] in
		let l = match breakpoint.bpcolumn with
			| BPAny -> l
			| BPColumn i -> ("column",JInt i) :: l
		in
		JObject l

	let output_breakpoint_description ctx breakpoint =
		output_result ctx (get_breakpoint_description ctx breakpoint)

	let output_breakpoint_set ctx breakpoint =
		output_result ctx (JInt breakpoint.bpid)

	let output_breakpoints ctx =
		let a = DynArray.create () in
		iter_breakpoints ctx (fun breakpoint ->
			DynArray.add a (get_breakpoint_description ctx breakpoint)
		);
		output_result ctx (JArray (DynArray.to_list a))

	let output_breakpoint_stop ctx env =
		output_event ctx "breakpoint_stop" None

	let output_exception_stop ctx v pos =
		output_event ctx "exception_stop" (Some (JObject ["text",JString (value_string v)]))

	let output_set_var_result ctx name value access =
		output_result ctx (var_to_json name value access)

	let output_scopes ctx capture_infos scopes =
		let mk_scope id name pos =
			let fl = ["id",JInt id; "name",JString name] in
			let fl =
				if pos <> null_pos then
					let line1,col1,line2,col2 = Lexer.get_pos_coords pos in
					("pos",JObject [
						"source",JString (Path.get_real_path pos.pfile);
						"line",JInt line1;
						"column",JInt col1;
						"endLine",JInt line2;
						"endColumn",JInt col2;
					]) :: fl
				else
					fl
			in
			JObject fl
		in
		let _,scopes = List.fold_left (fun (id,acc) scope ->
			if Hashtbl.length scope.local_infos <> 0 then
				(id + 1), (mk_scope id "Locals" scope.pos) :: acc
			else
				(id + 1), acc
		) (1,[]) scopes in
		let scopes = List.rev scopes in
		let scopes = if Hashtbl.length capture_infos = 0 then scopes else (mk_scope 0 "Captures" null_pos) :: scopes in
		output_result ctx (JArray scopes)

	let output_capture_vars ctx env =
		let infos = env.env_info.capture_infos in
		let vars = Hashtbl.fold (fun slot name acc ->
			let value = !(env.env_captures.(slot)) in
			(var_to_json name value name) :: acc
		) infos [] in
		output_result ctx (JArray vars)

	let output_scope_vars ctx env scope =
		let vars = Hashtbl.fold (fun local_slot name acc ->
			let slot = local_slot + scope.local_offset in
			let value = env.env_locals.(slot) in
			(var_to_json name value name) :: acc
		) scope.local_infos [] in
		output_result ctx (JArray vars)

	let output_inner_vars ctx v access =
		let children = match v with
			| VNull | VTrue | VFalse | VInt32 _ | VFloat _ | VFunction _ | VFieldClosure _ -> []
			| VEnumValue ve ->
				begin match ve.eargs with
					| [||] -> []
					| vl ->
						Array.to_list (Array.mapi (fun i v ->
							let n = Printf.sprintf "[%d]" i in
							let a = access ^ n in
							n, v, a
						) vl)
				end
			| VObject o ->
				let fields = object_fields o in
				List.map (fun (n,v) ->
					let n = rev_hash_s n in
					let a = access ^ "." ^ n in
					n, v, a
				) fields
			| VString(_,s) -> []
			| VArray va ->
				let l = EvalArray.to_list va in
				List.mapi (fun i v ->
					let n = Printf.sprintf "[%d]" i in
					let a = access ^ n in
					n, v, a
				) l
			| VInstance vi ->
				let fields = instance_fields vi in
				List.map (fun (n,v) ->
					let n = rev_hash_s n in
					let a = access ^ "." ^ n in
					n, v, a
				) fields
			| VPrototype proto -> [] (* TODO *)
		in
		let vars = List.map (fun (n,v,a) -> var_to_json n v a) children in
		output_result ctx (JArray vars)

end

module DebugInput = struct
	open Unix

	let read_byte this i = int_of_char (Bytes.get this i)

	let read_ui16 this i =
		let ch1 = read_byte this i in
		let ch2 = read_byte this (i + 1) in
		ch1 lor (ch2 lsl 8)

	let read_line ctx = match ctx.debug.debug_socket with
		| None ->
			input_line Pervasives.stdin
		| Some socket ->
			begin match socket.socket with
				| None ->
					input_line Pervasives.stdin
				| Some socket ->
					let buf = Bytes.create 2 in
					let _ = recv socket buf 0 2 [] in
					let i = read_ui16 buf 0 in
					let buf = Bytes.create i in
					let _ = recv socket buf 0 i [] in
					Bytes.to_string buf
			end
end

open DebugOutputJson

let print_variables ctx capture_infos scopes env =
	let rec loop scopes = match scopes with
		| scope :: scopes ->
			Hashtbl.iter (fun _ name -> output_variable_name ctx name) scope.local_infos;
			loop scopes
		| [] ->
			()
	in
	loop scopes;
	Hashtbl.iter (fun slot name ->
		if slot < Array.length env.env_captures then
			output_variable_name ctx name
	) capture_infos

let print_variable ctx capture_infos scopes name env =
	try
		let value = get_variable capture_infos scopes name env in
		output_value ctx name value
	with Not_found ->
		output_error ctx ("No variable found: " ^ name)

let set_variable ctx scopes name value env =
	try
		let slot = get_var_slot_by_name scopes name in
		env.env_locals.(slot) <- value;
		output_value ctx name value;
	with Not_found ->
		output_error ctx ("No variable found: " ^ name)

let parse_expr ctx s p =
	let msg = ref "" in
	let error s =
		msg := s;
		raise Exit
	in
	begin try
		let e = Parser.parse_expr_string (ctx.curapi.get_com()) s p error false in
		Some e
	with Exit ->
		output_error ctx (Printf.sprintf "Error parsing %s: %s" s !msg);
		None
	end

(* Checks debug state and calls what's needed. *)
let rec run_loop ctx run env : value =
	let check_breakpoint () =
		if ctx.debug.breakpoint.bpstate = BPHit && env.env_debug.line <> ctx.debug.breakpoint.bpline then ctx.debug.breakpoint.bpstate <- BPEnabled
	in
	match ctx.debug.debug_state with
		| DbgRunning ->
			check_breakpoint();
			run env
		| DbgContinue ->
			check_breakpoint();
			run env
		| DbgNext offset ->
			if offset < (get_eval ctx).environment_offset then
				run env
			else begin
				ctx.debug.debug_state <- DbgWaiting;
				run_loop ctx run env
			end
		| DbgFinish offset ->
			if offset <= (get_eval ctx).environment_offset then
				run env
			else begin
				ctx.debug.debug_state <- DbgWaiting;
				run_loop ctx run env
			end
		| DbgWaiting | DbgStart ->
			wait ctx run env

(* Reads input and reacts accordingly. *)
and wait ctx run env =
	let get_real_env ctx =
		ctx.debug.environment_offset_delta <- 0;
		DynArray.get (get_eval ctx).environments ((get_eval ctx).environment_offset - 1);
	in
	let rec move_frame offset : value =
		if offset < 0 || offset >= (get_eval ctx).environment_offset then begin
			output_error ctx (Printf.sprintf "Frame out of bounds: %i (valid range is %i - %i)" offset 0 ((get_eval ctx).environment_offset - 1));
			loop()
		end else begin
			ctx.debug.environment_offset_delta <- ((get_eval ctx).environment_offset - offset - 1);
			output_success ctx;
			wait ctx run (DynArray.get (get_eval ctx).environments offset);
		end
	and loop () =
		print_string "1> ";
		flush stdout;
		let line = DebugInput.read_line ctx in
		match ExtString.String.nsplit line " " with
		| ["quit" | "exit"] ->
			(* TODO: Borrowed from interp.ml *)
			if (get_ctx()).curapi.use_cache() then raise (Error.Fatal_error ("",Globals.null_pos));
			raise (Interp.Sys_exit 0);
		| ["detach"] ->
			Hashtbl.iter (fun _ h ->
				Hashtbl.clear h
			) ctx.debug.breakpoints;
			ctx.debug.debug_state <- DbgRunning;
			run env
		(* source | history *)
		| ["files" | "filespath"] ->
			Hashtbl.iter (fun i _ ->
				output_file_path ctx (rev_hash_s i);
			) ctx.debug.breakpoints;
			loop()
		| ["classes"] ->
			IntMap.iter (fun i _ ->
				output_type_name ctx (rev_hash_s i)
			) ctx.type_cache;
			loop()
		| ["mem"] ->
			output_info ctx (Printf.sprintf "%i" (Gc.stat()).live_words);
			loop()
		| ["compact"] ->
			let before = (Gc.stat()).live_words in
			Gc.compact();
			let after = (Gc.stat()).live_words in
			output_info ctx (Printf.sprintf "before: %i\nafter: %i" before after);
			loop()
		| ["collect"] ->
			let before = (Gc.stat()).live_words in
			Gc.full_major();
			let after = (Gc.stat()).live_words in
			output_info ctx (Printf.sprintf "before: %i\nafter: %i" before after);
			loop()
		| ["break" | "b";pattern] ->
			begin try
				let file,line,column = parse_breakpoint_pattern pattern in
				begin try
					let breakpoint = add_breakpoint ctx file line column in
					output_breakpoint_set ctx breakpoint;
				with Not_found ->
					output_error ctx ("Could not find file " ^ file);
				end;
			with Exit ->
				output_error ctx ("Unrecognized breakpoint pattern");
			end;
			loop()
		| ["list" | "l"] ->
			(* TODO: other list syntax *)
			output_breakpoints ctx;
			loop()
		| ["describe" | "desc";bpid] ->
			(* TODO: range patterns? *)
			begin try
				let breakpoint = find_breakpoint ctx bpid in
				output_breakpoint_description ctx breakpoint;
			with Not_found ->
				output_error ctx (Printf.sprintf "Unknown breakpoint: %s" bpid);
			end;
			loop()
		| ["disable" | "dis";bpid] ->
			(* TODO: range patterns? *)
			if bpid = "all" then
				iter_breakpoints ctx (fun breakpoint -> breakpoint.bpstate <- BPDisabled)
			else begin try
				let breakpoint = find_breakpoint ctx bpid in
				breakpoint.bpstate <- BPDisabled;
				output_info ctx (Printf.sprintf "Breakpoint %s disabled" bpid);
			with Not_found ->
				output_error ctx (Printf.sprintf "Unknown breakpoint: %s" bpid);
			end;
			loop()
		| ["enable" | "en";bpid] ->
			(* TODO: range patterns? *)
			if bpid = "all" then
				iter_breakpoints ctx (fun breakpoint -> breakpoint.bpstate <- BPEnabled)
			else begin try
				let breakpoint = find_breakpoint ctx bpid in
				breakpoint.bpstate <- BPEnabled;
				output_info ctx (Printf.sprintf "Breakpoint %s enabled" bpid);
			with Not_found ->
				output_error ctx (Printf.sprintf "Unknown breakpoint: %s" bpid);
			end;
			loop()
		| ["delete" | "d";bpid] ->
			(* TODO: range patterns? *)
			if bpid = "all" then
				Hashtbl.iter (fun _ h ->
					Hashtbl.clear h
				) ctx.debug.breakpoints
			else begin try
				let id = try int_of_string bpid with _ -> raise Not_found in
				Hashtbl.iter (fun _ h ->
					let to_delete = ref [] in
					Hashtbl.iter (fun k breakpoint -> if breakpoint.bpid = id then to_delete := k :: !to_delete) h;
					List.iter (fun k -> Hashtbl.remove h k) !to_delete;
				) ctx.debug.breakpoints;
				output_info ctx (Printf.sprintf "Breakpoint %s deleted" bpid);
			with Not_found ->
				output_error ctx (Printf.sprintf "Unknown breakpoint: %s" bpid);
			end;
			loop()
		| ["clear";pattern] ->
			(* TODO: range patterns? *)
			begin try
				let file,line,column = parse_breakpoint_pattern pattern in
				begin try
					delete_breakpoint ctx file line
				with Not_found ->
					output_info ctx (Printf.sprintf "Could not find breakpoint %s:%i" file line);
				end
			with Exit ->
				output_error ctx ("Unrecognized breakpoint pattern");
			end;
			loop()
		(* thread | unsafe | safe *)
		| ["continue" | "c"] ->
			let env = get_real_env ctx in
			ctx.debug.debug_state <- (if ctx.debug.debug_state = DbgStart then DbgRunning else DbgContinue);
			run env
		| ["step" | "s" | ""] ->
			let env = get_real_env ctx in
			run env
		| ["next" | "n"] ->
			let env = get_real_env ctx in
			ctx.debug.debug_state <- DbgNext (get_eval ctx).environment_offset;
			run env
		| ["finish" | "f"] ->
			let env = get_real_env ctx in
			ctx.debug.debug_state <- DbgFinish (get_eval ctx).environment_offset;
			run env
		| ["where" | "w"] ->
			output_call_stack ctx env.env_info.kind env.env_debug.expr.epos;
			loop()
		| ["up"] ->
			let offset = (get_eval ctx).environment_offset - ctx.debug.environment_offset_delta in
			move_frame (offset - 2)
		| ["down"] ->
			let offset = (get_eval ctx).environment_offset - ctx.debug.environment_offset_delta in
			move_frame offset
		| ["frame";sframe] ->
			let frame = try
				Some (int_of_string sframe)
			with _ ->
				None
			in
			begin match frame with
				| Some frame -> move_frame ((get_eval ctx).environment_offset - frame - 1)
				| None ->
					output_error ctx ("Invalid frame format: " ^ sframe);
					loop()
			end
		| ["scopes"] ->
			output_scopes ctx env.env_info.capture_infos env.env_debug.scopes;
			loop()
		| ["variables" | "vars"] ->
			print_variables ctx env.env_info.capture_infos env.env_debug.scopes env;
			loop()
		| ["vars";sid] ->
			begin
				try
					let sid = try int_of_string sid with _ -> raise Exit in
					if sid = 0 then begin
						output_capture_vars ctx env;
						loop();
					end else begin
						let scope = try List.nth env.env_debug.scopes (sid - 1) with _ -> raise Exit in
						output_scope_vars ctx env scope;
						loop()
					end
				with Exit -> begin
					output_error ctx ("Invalid scope id");
					loop ()
				end
			end
		| ["structure";e] ->
			begin match parse_expr ctx e env.env_debug.expr.epos with
				| Some e ->
					begin try
						let access,v = expr_to_value ctx env e in
						output_inner_vars ctx v access
					with Exit ->
						output_error ctx ("Don't know how to handle this expression: " ^ (Ast.s_expr e))
					end
				| None ->
					()
			end;
			loop()
		| ["print" | "p";e] ->
			begin match parse_expr ctx e env.env_debug.expr.epos with
				| Some e ->
					begin try
						let name,v = expr_to_value ctx env e in
						output_value ctx name v
					with Exit ->
						output_error ctx ("Don't know how to handle this expression: " ^ (Ast.s_expr e))
					end
				| None ->
					()
			end;
			loop()
		| ["set" | "s";expr_s;"=";value] ->
			let parse s = parse_expr ctx s env.env_debug.expr.epos in
			begin match parse expr_s,parse value with
				| Some expr,Some value ->
					begin try
						let _,value = expr_to_value ctx env value in
						begin match fst expr with
							(* TODO: support setting array elements and enum values *)
							| EField(e1,s) ->
								let _,v1 = expr_to_value ctx env e1 in
								set_field v1 (hash_s s) value;
								output_set_var_result ctx s value expr_s
							| EConst (Ident s) ->
								set_variable ctx env.env_debug.scopes s value env;
								output_set_var_result ctx s value expr_s
							| _ ->
								raise Exit
						end
					with Exit ->
						output_error ctx ("Don't know how to handle this expression")
					end
				| _ ->
					()
			end;
			loop()
		| s ->
			output_error ctx (Printf.sprintf "Unknown command: %s" (String.concat " " s));
			loop()
	in
	loop ()

let debug_loop jit e f =
	let ctx = jit.ctx in
	let scopes = jit.scopes in
	let line,col1,_,_ = Lexer.get_pos_coords e.epos in
	let column_matches breakpoint = match breakpoint.bpcolumn with
		| BPAny -> true
		| BPColumn i -> i = col1 + 1
	in
	(* Checks if we hit a breakpoint, runs the code if not. *)
	let rec run_check_breakpoint env =
		try
			let h = Hashtbl.find ctx.debug.breakpoints env.env_info.pfile in
			let breakpoint = Hashtbl.find h env.env_debug.line in
			begin match breakpoint.bpstate with
				| BPEnabled when column_matches breakpoint ->
					breakpoint.bpstate <- BPHit;
					ctx.debug.breakpoint <- breakpoint;
					output_breakpoint_stop ctx env;
					ctx.debug.debug_state <- DbgWaiting;
					run_loop ctx run_check_breakpoint env
				| _ ->
					raise Not_found
			end
		with Not_found -> try
			f env
		with RunTimeException(v,_,_) when not (is_caught ctx v) ->
			output_exception_stop ctx v e.epos;
			ctx.debug.debug_state <- DbgWaiting;
			run_loop ctx run_check_breakpoint env
	in
	(* Sets the environmental debug data, then executes the debug loop. *)
	let run_set env =
		env.env_debug.scopes <- scopes;
		env.env_debug.line <- line;
		env.env_debug.expr <- e;
		run_loop ctx run_check_breakpoint env;
	in
	run_set