open Gc
open Ast
open Type
open Globals
open MacroApi
open JsonRpcSocket
open Json
open EvalContext
open EvalValue
open EvalHash
open EvalPrinting
open EvalMisc
open EvalDebugMisc

(* Printing *)

let var_to_json name value vio env =
	let jv t v num_children =
		let id = if num_children = 0 then 0 else (get_ctx()).debug.debug_context#add_value value env in
		let fields = [
			"id",JInt id;
			"name",JString name;
			"type",JString t;
			"value",JString v;
			"numChildren",JInt num_children;
		] in
		let fields = match vio with
			| Some vi ->
				let line,col1,_,_ = Lexer.get_pos_coords vi.vi_pos in
				("generated",JBool vi.vi_generated) ::
				("line",JInt line) ::
				("column",JInt col1) ::
				fields
			| None ->
				fields
		in
		JObject fields
	in
	let string_repr s = "\"" ^ (StringHelper.s_escape s.sstring) ^ "\"" in
	let rec level2_value_repr = function
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
		| VString s -> string_repr s
		| VArray _ | VVector _ -> "[...]"
		| VInstance vi -> (rev_hash vi.iproto.ppath) ^ " {...}"
		| VPrototype proto -> (s_proto_kind proto).sstring
		| VFunction _ | VFieldClosure _ -> "<fun>"
		| VLazy f -> level2_value_repr (!f())
	in
	let fields_string fields =
		let l = List.map (fun (name, value) -> Printf.sprintf "%s: %s" (rev_hash name) (level2_value_repr value)) fields in
		Printf.sprintf "{%s}" (String.concat ", " l)
	in
	let array_elems l =
		let l = List.map level2_value_repr l in
		Printf.sprintf "[%s]" (String.concat ", " l)
	in
	let rec value_string v = match v with
		| VNull -> jv "NULL" "null" 0
		| VTrue -> jv "Bool" "true" 0
		| VFalse -> jv "Bool" "false" 0
		| VInt32 i -> jv "Int" (Int32.to_string i) 0
		| VFloat f -> jv "Float" (string_of_float f) 0
		| VEnumValue ve ->
			let type_s = rev_hash ve.epath in
			let name = EvalPrinting.s_enum_ctor_name ve in
			let value_s = match ve.eargs with
				| [||] -> name
				| vl ->
					let l = Array.to_list (Array.map level2_value_repr vl) in
					let s = Printf.sprintf "%s(%s)" name (String.concat ", " l) in
					s
			in
			jv type_s value_s (Array.length ve.eargs)
		| VObject o ->
			let fields = object_fields o in
			jv "Anonymous" (fields_string fields) (List.length fields)
		| VString s ->
			jv "String" (string_repr s) 2
		| VArray va -> jv "Array" (array_elems (EvalArray.to_list va)) va.alength
		| VVector vv -> jv "Vector" (array_elems (Array.to_list vv)) (Array.length vv)
		| VInstance vi ->
			let class_name = EvalDebugMisc.safe_call (get_ctx()) EvalPrinting.value_string v in
			let fields = instance_fields vi in
			jv class_name (class_name) (List.length fields)
		| VPrototype proto ->
			let fields = proto_fields proto in
			jv "Anonymous" (s_proto_kind proto).sstring (List.length fields)
		| VFunction _ | VFieldClosure _ -> jv "Function" "<fun>" 0
		| VLazy f -> value_string (!f())
	in
	value_string value

let get_call_stack_envs ctx kind p =
	let envs = match call_stack ctx with
		| _ :: envs -> envs
		| [] -> []
	in
	let rec loop delta envs = match envs with
		| _ :: envs when delta < 0 -> loop (delta + 1) envs
		| _ -> envs
	in
	loop 0 envs

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
		let p = {pmin = env.env_leave_pmin; pmax = env.env_leave_pmax; pfile = rev_file_hash env.env_info.pfile} in
		(stack_item env.env_info.kind p (env.env_leave_pmin < 0)) :: acc
	) l envs in
	JArray (List.rev stack)

let output_scopes ctx env =
	let capture_infos = env.env_info.capture_infos in
	let scopes = env.env_debug.scopes in
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
	let scopes = List.fold_left (fun acc scope ->
		if Hashtbl.length scope.local_infos <> 0 then
			(mk_scope (ctx.debug.debug_context#add_scope scope env) "Locals" scope.pos) :: acc
		else
			acc
	) [] scopes in
	let scopes = List.rev scopes in
	let scopes = if Hashtbl.length capture_infos = 0 then scopes else (mk_scope (ctx.debug.debug_context#add_capture_scope capture_infos env) "Captures" null_pos) :: scopes in
	JArray scopes

let output_capture_vars infos env =
	let vars = Hashtbl.fold (fun slot vi acc ->
		let value = !(env.env_captures.(slot)) in
		(var_to_json vi.vi_name value (Some vi) env) :: acc
	) infos [] in
	JArray vars

let output_scope_vars env scope =
	let vars = Hashtbl.fold (fun local_slot vi acc ->
		let slot = local_slot + scope.local_offset in
		let value = env.env_locals.(slot) in
		(var_to_json vi.vi_name value (Some vi) env) :: acc
	) scope.local_infos [] in
	JArray vars

let output_inner_vars v env =
	let rec loop v = match v with
		| VNull | VTrue | VFalse | VInt32 _ | VFloat _ | VFunction _ | VFieldClosure _ -> []
		| VEnumValue ve ->
			begin match (get_static_prototype_raise (get_ctx()) ve.epath).pkind with
				| PEnum names ->
					let fields = snd (List.nth names ve.eindex) in
					List.mapi (fun i n ->
						let n = rev_hash n in
						let v = ve.eargs.(i) in
						n, v
					) fields
				| _ -> []
			end
		| VObject o ->
			let fields = object_fields o in
			List.map (fun (n,v) ->
				let n = rev_hash n in
				n, v
			) fields
		| VString s -> [
			"length",vint s.slength;
			"byteLength",vint (String.length s.sstring);
		]
		| VArray va ->
			let l = EvalArray.to_list va in
			List.mapi (fun i v ->
				let n = Printf.sprintf "[%d]" i in
				n, v
			) l
		| VVector vv ->
			let l = Array.to_list vv in
			List.mapi (fun i v ->
				let n = Printf.sprintf "[%d]" i in
				n, v
			) l
		| VInstance {ikind = IStringMap h} ->
			StringHashtbl.fold (fun s (_,v) acc ->
				(s,v) :: acc
			) h []
		| VInstance vi ->
			let fields = instance_fields vi in
			List.map (fun (n,v) ->
				let n = rev_hash n in
				n, v
			) fields
		| VPrototype proto ->
			let fields = proto_fields proto in
			List.map (fun (n,v) ->
				let n = rev_hash n in
				n, v
			) fields
		| VLazy f -> loop (!f())
	in
	let children = loop v in
	let vars = List.map (fun (n,v) -> var_to_json n v None env) children in
	JArray vars

type command_outcome =
	| Loop of Json.t
	| Run of Json.t * EvalContext.env
	| Wait of Json.t * EvalContext.env

module ValueCompletion = struct
	let prototype_instance_fields proto =
		let rec loop acc proto =
			let acc = match proto.pparent with
				| None -> acc
				| Some  proto -> loop acc proto
			in
			let acc = IntMap.fold (fun name _ acc -> IntMap.add name (name,"field",None) acc) proto.pinstance_names acc in
			IntMap.fold (fun name _ acc -> IntMap.add name (name,"method",None) acc) proto.pnames acc
		in
		loop IntMap.empty proto

	let prototype_static_fields proto =
		IntMap.fold (fun name _ acc -> IntMap.add name (name,"field",None) acc) proto.pnames IntMap.empty


	let to_json l =
		JArray (List.map (fun (n,k,column) ->
			let fields = ["label",JString (rev_hash n);"kind",JString k] in
			let fields = match column with None -> fields | Some column -> ("start",JInt column) :: fields in
			JObject fields
		) l)

	let collect_idents ctx env =
		let acc = Hashtbl.create 0 in
		let add key =
			if not (Hashtbl.mem acc key) then
				Hashtbl.add acc key (key,"value",None)
		in
		(* 0. Extra locals *)
		IntMap.iter (fun key _ -> add key) env.env_extra_locals;
		(* 1. Variables *)
		let rec loop scopes = match scopes with
			| scope :: scopes ->
				Hashtbl.iter (fun key _ -> add (hash key)) scope.local_ids;
				loop scopes
			| [] ->
				()
		in
		loop env.env_debug.scopes;
		(* 2. Captures *)
		Hashtbl.iter (fun slot vi ->
			add (hash vi.vi_name)
		) env.env_info.capture_infos;
		(* 3. Instance *)
		if not env.env_info.static then begin
			let v = env.env_locals.(0) in
			begin match v with
			| VInstance vi ->
				let fields = prototype_instance_fields vi.iproto in
				IntMap.iter (fun key _ -> add key) fields
			| _ ->
				()
			end
		end;
		(* 4. Static *)
		begin match env.env_info.kind with
			| EKMethod(i1,_) ->
				let proto = get_static_prototype_raise ctx i1 in
				let fields = prototype_static_fields proto in
				IntMap.iter (fun key _ -> add key) fields
			| _ ->
				raise Not_found
		end;
		(* 5. Toplevel *)
		begin match ctx.toplevel with
		| VObject o ->
			let fields = object_fields o in
			List.iter (fun (n,_) -> add n) fields
		| _ ->
			()
		end;
		to_json (Hashtbl.fold (fun _ v acc -> v :: acc) acc [])

	let output_completion ctx column v =
		let field_value_kind v = match v with
			| VFunction _ | VFieldClosure _ -> "function"
			| _ -> "field"
		in
		let rec loop v = match v with
			| VNull | VTrue | VFalse | VInt32 _ | VFloat _ | VFunction _ | VFieldClosure _ ->
				[]
			| VObject o ->
				let fields = object_fields o in
				List.map (fun (n,v) ->
					n,(field_value_kind v),None
				) fields
			| VInstance vi ->
				let fields = prototype_instance_fields vi.iproto in
				IntMap.fold (fun _ v acc -> v :: acc) fields []
			| VString _ ->
				let fields = prototype_instance_fields ctx.string_prototype in
				IntMap.fold (fun _ v acc -> v :: acc) fields [key_length,"field",None]
			| VArray _ ->
				let fields = prototype_instance_fields ctx.array_prototype in
				IntMap.fold (fun _ v acc -> v :: acc) fields [key_length,"field",None]
			| VVector _ ->
				let fields = prototype_instance_fields ctx.vector_prototype in
				IntMap.fold (fun _ v acc -> v :: acc) fields [key_length,"field",None]
			| VPrototype proto ->
				let fields = prototype_static_fields proto in
				IntMap.fold (fun _ v acc -> v :: acc) fields []
			| VLazy f ->
				loop (!f())
			| VEnumValue ve ->
				begin match (get_static_prototype_raise (get_ctx()) ve.epath).pkind with
					| PEnum names ->
						let fields = snd (List.nth names ve.eindex) in
						List.map (fun n -> n,"value",None) fields
					| _ -> []
				end
		in
		let l = loop v in
		to_json l

	exception JsonException of Json.t

	let get_completion ctx text column env =
		let p = { pmin = 0; pmax = 0; pfile = "" } in
		let save =
			let old = !Parser.display_mode,!DisplayPosition.display_position in
			(fun () ->
				Parser.display_mode := fst old;
				DisplayPosition.display_position := snd old;
			)
		in
		Parser.display_mode := DMDefault;
		let offset = column + (String.length "class X{static function main() ") - 1 (* this is retarded *) in
		DisplayPosition.display_position := {p with pmin = offset; pmax = offset};
		begin try
			let e = parse_expr ctx text p in
			let e = Display.ExprPreprocessing.find_before_pos DMDefault e in
			save();
			let rec loop e = match fst e with
			| EDisplay(e1,DKDot) ->
				let v = expr_to_value ctx env e1 in
				let json = output_completion ctx column v in
				raise (JsonException json)
			| EArray(e1,(EDisplay((EConst (Ident "null"),_),DKMarked),_)) ->
				let v = expr_to_value ctx env e1 in
				begin match v with
				| VArray va ->
					let l = EvalArray.to_list va in
					let l = List.mapi (fun i v ->
						let n = Printf.sprintf "%d" i in
						(hash n),"value",Some column
					) l in
					raise (JsonException (to_json l))
				| _ ->
					raise Exit
				end
			| EDisplay(e1,DKMarked) ->
				let json = collect_idents ctx env in
				raise (JsonException json)
			| _ ->
				Ast.iter_expr loop e
			in
			begin try
				loop e;
				raise Exit
			with
			| JsonException json ->
				Loop (json)
			end
		with _ ->
			save();
			raise Exit
		end;

end

type handler_context = {
	ctx : context;
	jsonrpc : Jsonrpc_handler.jsonrpc_handler;
	env : env;
	send_error : 'a . string -> 'a;
}

let handler =
	let get_real_env ctx =
		(get_eval ctx).env
	in
	let parse_breakpoint hctx jo =
		let j = hctx.jsonrpc in
		let obj = j#get_object "breakpoint" jo in
		let line = j#get_int_field "line" "line" obj in
		let column = j#get_opt_param (fun () -> BPColumn (j#get_int_field "column" "column" obj)) BPAny in
		let condition = j#get_opt_param (fun () ->
			let s = j#get_string_field "condition" "condition" obj in
			Some (parse_expr hctx.ctx s hctx.env.env_debug.expr.epos)
		) None in
		(line,column,condition)
	in
	let rec move_frame hctx offset =
		let eval = get_eval hctx.ctx in
		let rec loop env i =
			if i = 0 then env
			else match env.env_parent with
			| None -> hctx.send_error "Frame out of bounds"
			| Some env -> loop env (i - 1)
		in
		if offset < 0 then
			hctx.send_error "Frame out of bounds"
		else begin
			loop eval.env offset
		end
	in
	let update_frame hctx =
		let frame = hctx.jsonrpc#get_int_param "frameId" in
		move_frame hctx frame
	in
	let h = Hashtbl.create 0 in
	let l = [
		"continue",(fun hctx ->
			let env = get_real_env hctx.ctx in
			hctx.ctx.debug.debug_state <- (if hctx.ctx.debug.debug_state = DbgStart then DbgRunning else DbgContinue);
			Run (JNull,env)
		);
		"stepIn",(fun hctx ->
			let env = get_real_env hctx.ctx in
			Run (JNull,env)
		);
		"next",(fun hctx ->
			let env = get_real_env hctx.ctx in
			hctx.ctx.debug.debug_state <- DbgNext(env,env.env_debug.expr.epos);
			Run (JNull,env)
		);
		"stepOut",(fun hctx ->
			let env = get_real_env hctx.ctx in
			let penv = Option.get env.env_parent in
			hctx.ctx.debug.debug_state <- DbgFinish penv;
			Run (JNull,env)
		);
		"stackTrace",(fun hctx ->
			Loop (output_call_stack hctx.ctx hctx.env.env_info.kind hctx.env.env_debug.expr.epos)
		);
		"getScopes",(fun hctx ->
			let env = update_frame hctx in
			Loop (output_scopes hctx.ctx env);
		);
		"getScopeVariables",(fun hctx ->
			let sid = hctx.jsonrpc#get_int_param "id" in
			begin
				let vars =
					try
						begin
							let scope = hctx.ctx.debug.debug_context#get sid in
							match scope with
							| Scope(scope,env) ->
								output_scope_vars env scope
							| CaptureScope(infos,env) ->
								output_capture_vars infos env
							| Value(value,env) ->
								output_inner_vars value env
							| Toplevel ->
								hctx.send_error "Invalid scope id";
						end
					with Exit ->
						hctx.send_error "Invalid scope id"
				in
				Loop vars
			end
		);
		"setBreakpoints",(fun hctx ->
			let file = hctx.jsonrpc#get_string_param "file" in
			let bps = hctx.jsonrpc#get_array_param "breakpoints" in
			let bps = List.map (parse_breakpoint hctx) bps in
			let hash = hash (Path.unique_full_path (Common.find_file (hctx.ctx.curapi.get_com()) file)) in
			let h =
				try
					let h = Hashtbl.find hctx.ctx.debug.breakpoints hash in
					Hashtbl.clear h;
					h
				with Not_found ->
					let h = Hashtbl.create (List.length bps) in
					Hashtbl.add hctx.ctx.debug.breakpoints hash h;
					h
			in
			let bps = List.map (fun (line,column,condition) ->
				let bp = make_breakpoint hash line BPEnabled column condition in
				Hashtbl.add h line bp;
				JObject ["id",JInt bp.bpid]
			) bps in
			Loop (JArray bps)
		);
		"setBreakpoint",(fun hctx ->
			let line,column,condition = parse_breakpoint hctx (hctx.jsonrpc#get_params) in
			let file = hctx.jsonrpc#get_string_param "file" in
			let breakpoint = add_breakpoint hctx.ctx file line column condition in
			Loop (JObject ["id",JInt breakpoint.bpid])
		);
		"setFunctionBreakpoints",(fun hctx ->
			Hashtbl.clear hctx.ctx.debug.function_breakpoints;
			let j = hctx.jsonrpc in
			let bps = j#get_array "param" (j#get_params) in
			let bps = List.map (fun jo ->
				let obj = j#get_object "breakpoint" jo in
				let name = j#get_string_field "name" "name" obj in
				let i = String.rindex name '.' in
				if i < 0 then hctx.send_error "Invalid format, expected Type.field";
				let key_type = String.sub name 0 i in
				let key_field = String.sub name (i + 1) (String.length name - i - 1) in
				let bp = make_function_breakpoint BPEnabled in
				Hashtbl.add hctx.ctx.debug.function_breakpoints (hash key_type,hash key_field) bp;
				JObject ["id",JInt bp.fbpid]
			) bps in
			Loop (JArray bps)
		);
		"removeBreakpoint",(fun hctx ->
			let id = hctx.jsonrpc#get_int_param "id" in
			begin try
				Hashtbl.iter (fun _ h ->
					let to_delete = ref [] in
					Hashtbl.iter (fun k breakpoint -> if breakpoint.bpid = id then to_delete := k :: !to_delete) h;
					List.iter (fun k -> Hashtbl.remove h k) !to_delete;
				) hctx.ctx.debug.breakpoints;
			with Not_found ->
				hctx.send_error (Printf.sprintf "Unknown breakpoint: %d" id)
			end;
			Loop JNull
		);
		"setVariable",(fun hctx ->
			let env = hctx.env in
			let id = hctx.jsonrpc#get_int_param "id" in
			let name = hctx.jsonrpc#get_string_param "name" in
			let value = hctx.jsonrpc#get_string_param "value" in
			let value = try
				let e = parse_expr hctx.ctx value env.env_debug.expr.epos in
				expr_to_value hctx.ctx env e
			with Parse_expr_error e ->
				hctx.send_error e
			in
			begin match hctx.ctx.debug.debug_context#get id with
			| Toplevel ->
				hctx.send_error "Invalid id";
			| Value(v,env) ->
				let name_as_index () = try
					(* The name is [1] so we have to extract the number. This is quite stupid but not really our fault... *)
					int_of_string (String.sub name 1 (String.length name - 2))
				with _ ->
					hctx.send_error "integer expected"
				in
				begin match v with
				| VArray va -> EvalArray.set va (name_as_index()) value
				| VVector vv -> Array.set vv (name_as_index()) value
				| _ ->
					set_field v (hash name) value;
				end;
				Loop (var_to_json "" value None env)
			| Scope(scope,env) ->
				let id = Hashtbl.find scope.local_ids name in
				let slot = Hashtbl.find scope.locals id in
				env.env_locals.(slot + scope.local_offset) <- value;
				Loop (var_to_json "" value None env)
			| CaptureScope(infos,env) ->
				let slot = get_capture_slot_by_name infos name in
				env.env_captures.(slot) := value;
				Loop (var_to_json "" value None env)
			end
		);
		"setExceptionOptions",(fun hctx ->
			let sl = hctx.jsonrpc#get_array "" hctx.jsonrpc#get_params in
			let sl = List.map (hctx.jsonrpc#get_string "") sl in
			hctx.ctx.debug.exception_mode <- if List.mem "all" sl then CatchAll
				else if List.mem "uncaught" sl then CatchUncaught
				else CatchNone;
			Loop(JNull)
		);
		"evaluate",(fun hctx ->
			let env = update_frame hctx in
			let s = hctx.jsonrpc#get_string_param "expr" in
			begin try
				let e = parse_expr hctx.ctx s env.env_debug.expr.epos in
				let v = expr_to_value hctx.ctx env e in
				Loop (var_to_json "" v None env)
			with
			| Parse_expr_error e ->
				hctx.send_error e
			| Exit ->
				hctx.send_error "Don't know how to handle this expression"
			end
		);
		"getCompletion",(fun hctx ->
			let env = hctx.env in
			let text = hctx.jsonrpc#get_string_param "text" in
			let column = hctx.jsonrpc#get_int_param "column" in
			try
				ValueCompletion.get_completion hctx.ctx text column env
			with Exit ->
				hctx.send_error "No completion point found";
		);
	] in
	List.iter (fun (s,f) -> Hashtbl.add h s f) l;
	h

let make_connection socket =
	let output_breakpoint_stop ctx _ =
		ctx.debug.debug_context <- new eval_debug_context;
		send_event socket "breakpointStop" None
	in
	let output_exception_stop ctx v _ =
		ctx.debug.debug_context <- new eval_debug_context;
		send_event socket "exceptionStop" (Some (JObject ["text",JString (value_string v)]))
	in
	let rec wait ctx (run : env -> value) env =
		let rec process_outcome id outcome =
			let output j = send_json socket (JsonRpc.result id j) in
			match outcome with
			| Loop result ->
				output result;
				loop ()
			| Run (result,env) ->
				output result;
				run env
			| Wait (result,env) ->
				output result;
				wait ctx run env;
		and send_output_and_continue json =
			send_json socket json;
			loop ()
		and send_output_and_exit json =
			send_json socket json;
			raise Exit
		and loop () : value =
			let input = Socket.read_string socket in
			let input =
				JsonRpc.handle_jsonrpc_error (fun () -> JsonRpc.parse_request input) send_output_and_exit
			in
			let jsonrpc = new Jsonrpc_handler.jsonrpc_handler input in
			let error msg =
				let open JsonRpc in
				raise (JsonRpc_error (Custom (jsonrpc#get_id, 1, msg)))
			in
			let hctx = {
				ctx = ctx;
				jsonrpc = jsonrpc;
				env = env;
				send_error = error;
			} in
			JsonRpc.handle_jsonrpc_error (fun () ->
				let method_name = jsonrpc#get_method_name in
				let f = try
					Hashtbl.find handler method_name
				with Not_found ->
					JsonRpc.raise_method_not_found jsonrpc#get_id method_name
				in
				process_outcome jsonrpc#get_id (f hctx)
			) send_output_and_continue
		in
		try
			loop ()
		with Exit ->
			loop ()
	in
	{
		wait = wait;
		bp_stop = output_breakpoint_stop;
		exc_stop = output_exception_stop;
	}
