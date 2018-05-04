open Ast
open Globals
open MacroApi
open EvalContext
open EvalHash
open EvalValue
open EvalEncode
open EvalMisc

type debug_connection = {
	wait : context -> (env -> value) -> env -> value;
	bp_stop : context -> env -> unit;
	exc_stop : context -> value -> pos -> unit;
}

exception BreakHere

(* Breakpoints *)

let make_breakpoint =
	let id = ref (-1) in
	(fun file line state column condition ->
		incr id;
		{
			bpid = !id;
			bpfile = file;
			bpline = line;
			bpstate = state;
			bpcolumn = column;
			bpcondition = condition
		}
	)

let iter_breakpoints ctx f =
	Hashtbl.iter (fun _ breakpoints ->
		Hashtbl.iter (fun _ breakpoint -> f breakpoint) breakpoints
	) ctx.debug.breakpoints

let add_breakpoint ctx file line column condition =
	let hash = hash_s (Path.unique_full_path (Common.find_file (ctx.curapi.get_com()) file)) in
	let h = try
		Hashtbl.find ctx.debug.breakpoints hash
	with Not_found ->
		let h = Hashtbl.create 0 in
		Hashtbl.add ctx.debug.breakpoints hash h;
		h
	in
	let breakpoint = make_breakpoint hash line BPEnabled column condition in
	Hashtbl.replace h line breakpoint;
	breakpoint

let delete_breakpoint ctx file line =
	let hash = hash_s (Path.unique_full_path (Common.find_file (ctx.curapi.get_com()) file)) in
	let h = Hashtbl.find ctx.debug.breakpoints hash in
	Hashtbl.remove h line

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


(* Helper *)

exception Parse_expr_error of string

let parse_expr ctx s p =
	let error s = raise (Parse_expr_error s) in
	ParserEntry.parse_expr_string (ctx.curapi.get_com()).Common.defines s p error false

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

let rec expr_to_value ctx env e =
	let rec loop e = match fst e with
		| EConst cst ->
			begin match cst with
				| String s -> encode_string s
				| Int s -> VInt32 (Int32.of_string s)
				| Float s -> VFloat (float_of_string s)
				| Ident "true" -> VTrue
				| Ident "false" -> VFalse
				| Ident "null" -> VNull
				| Ident s ->
					let value = resolve_ident ctx env s in
					value
				| _ -> raise Exit
			end
		| EArray(e1,eidx) ->
			let v1 = loop e1 in
			let vidx = loop eidx in
			let idx = match vidx with VInt32 i -> Int32.to_int i | _ -> raise Exit in
			begin match v1 with
				| VArray va -> EvalArray.get va idx
				| VVector vv -> Array.get vv idx
				| VEnumValue ev -> Array.get ev.eargs idx
				| _ -> raise Exit
			end
		| EField(e1,s) ->
			let v1 = loop e1 in
			let v = EvalField.field v1 (hash_s s) in
			v
		| EArrayDecl el ->
			let vl = List.map loop el in
			encode_array vl
		| EObjectDecl fl ->
			let fl = List.map (fun ((s,_,_),e) -> s,loop e) fl in
			encode_obj_s None fl
		| EBinop(op,e1,e2) ->
			begin match op with
			| OpAssign ->
				let v2 = loop e2 in
				write_expr ctx env e1 v2;
				v2
			| OpAssignOp op ->
				raise Exit (* Nobody does that, right? *)
			| OpBoolAnd ->
				if is_true (loop e1) then loop e2
				else VFalse
			| OpBoolOr ->
				if is_true (loop e1) then VTrue
				else loop e2
			| _ ->
				let v1 = loop e1 in
				let v2 = loop e2 in
				let p = pos e in
				(get_binop_fun op p) v1 v2
			end
		| EUnop(op,flag,e1) ->
			begin match op with
			| Not ->
				begin match loop e1 with
				| VNull | VFalse -> VTrue
				| _ -> VFalse
				end
			| Neg ->
				begin match loop e1 with
				| VFloat f -> VFloat (-.f)
				| VInt32 i -> vint32 (Int32.neg i)
				| _ -> raise Exit
				end
			| NegBits ->
				op_sub (pos e) (vint32 (Int32.minus_one)) (loop e1)
			| Increment | Decrement ->
				raise Exit
			end
		| ECall(e1,el) ->
			let v1 = loop e1 in
			let vl = List.map loop el in
			call_value v1 vl
		| EBlock el ->
			let rec loop2 el = match el with
				| [] -> VNull
				| [e1] -> loop e1
				| e1 :: el ->
					ignore(loop e1);
					loop2 el
			in
			loop2 el
		| EIf(e1,e2,eo) ->
			let v1 = loop e1 in
			if is_true v1 then loop e2
			else Option.map_default loop VNull eo
		| ETernary(e1,e2,e3) ->
			let v1 = loop e1 in
			if is_true v1 then loop e2 else loop e3
		| EParenthesis e1 | EMeta(_,e1) | EUntyped e1 | ECast(e1,None) | ECheckType(e1,_) ->
			loop e1
		| EWhile _ | ETry _ | ESwitch _ | ENew _ | EVars _ | EFunction _ | EFor _ | EDisplay _
		| EDisplayNew _ | EReturn _ | EContinue | EBreak | EThrow _  | ECast(_,Some _) ->
			raise Exit
	in
	loop e

and write_expr ctx env expr value =
	begin match fst expr with
		| EField(e1,s) ->
			let v1 = expr_to_value ctx env e1 in
			set_field v1 (hash_s s) value;
		| EConst (Ident s) ->
			begin try
				let slot = get_var_slot_by_name env.env_debug.scopes s in
				env.env_locals.(slot) <- value;
			with Not_found ->
				raise Exit
			end
		| EArray(e1,e2) ->
			let v1 = expr_to_value ctx env e1 in
			let vidx = expr_to_value ctx env e2 in
			let idx = match vidx with VInt32 i -> Int32.to_int i | _ -> raise Exit in
			begin match v1 with
				| VArray va -> EvalArray.set va idx value
				| VVector vv -> Array.set vv idx value
				| VEnumValue ev -> Array.set ev.eargs idx value
				| _ -> raise Exit
			end
		| _ ->
			raise Exit
	end

let expr_to_value_safe ctx env e =
	try expr_to_value ctx env e
	with Exit -> VNull