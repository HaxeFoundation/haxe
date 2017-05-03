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

let make_breakpoint =
	let id = ref (-1) in
	(fun file line state ->
		incr id;
		{
			bpid = !id;
			bpfile = file;
			bpline = line;
			bpstate = state
		}
	)

let add_breakpoint ctx file line =
	let hash = hash_s (Path.unique_full_path (Common.find_file (ctx.curapi.get_com()) file)) in
	let h = try
		Hashtbl.find ctx.debug.breakpoints hash
	with Not_found ->
		let h = Hashtbl.create 0 in
		Hashtbl.add ctx.debug.breakpoints hash h;
		h
	in
	Hashtbl.replace h line (make_breakpoint hash line BPEnabled)

let delete_breakpoint ctx file line =
	let hash = hash_s (Path.unique_full_path (Common.find_file (ctx.curapi.get_com()) file)) in
	let h = Hashtbl.find ctx.debug.breakpoints hash in
	Hashtbl.remove h line

let print_locals jit scopes env =
	let rec loop scopes = match scopes with
		| scope :: scopes ->
			Hashtbl.iter (fun _ name ->
				print_endline name
			) scope.local_names;
			loop scopes
		| [] ->
			()
	in
	loop scopes

let get_var_slot_by_name scopes name =
	let rec loop scopes = match scopes with
		| scope :: scopes ->
			begin try
				let id = Hashtbl.find scope.local_ids name in
				let slot = Hashtbl.find scope.locals id + scope.local_offset in
				slot
			with Not_found ->
				loop scopes
			end
		| [] ->
			raise Not_found
	in
	loop scopes

let print_variable scopes name env =
	try
		let slot = get_var_slot_by_name scopes name in
		let value = env.locals.(slot) in
		print_endline (value_string value);
	with Not_found ->
		print_endline ("No variable found: " ^ name)

let set_variable scopes name value env =
	try
		let slot = get_var_slot_by_name scopes name in
		env.locals.(slot) <- value;
		print_endline (Printf.sprintf "set variable %s = %s" name (value_string value));
	with Not_found ->
		print_endline ("No variable found: " ^ name)

let print_call_stack ctx p =
	let envs = match call_stack ctx with
		| _ :: envs -> envs
		| [] -> []
	in
	print_endline (format_pos p);
	List.iter (fun env ->
		let p = {pmin = env.leave_pmin; pmax = env.leave_pmax; pfile = rev_hash_s env.info.pfile} in
		print_endline (format_pos p)
	) envs

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
		let file,line = ExtString.String.split pattern ":" in
		let line = int_of_string line in
		file,line
	with _ ->
		raise Exit

let rec expr_to_value e = match fst e with
	| EConst cst ->
		begin match cst with
			| String s -> encode_string s
			| Int s -> VInt32 (Int32.of_string s)
			| Float s -> VFloat (float_of_string s)
			| _ -> raise Exit
		end
	| _ ->
		raise Exit

let is_caught ctx v =
	let rec loop offset =
		if offset < 0 then false else begin
			let env = DynArray.get ctx.environments offset in
			List.exists (fun path -> is v path) env.info.caught_types || loop (offset - 1)
		end
	in
	loop (ctx.environment_offset - 1)

let debug_loop jit e f =
	let ctx = jit.ctx in
	let scopes = jit.scopes in
	let line = Lexer.get_error_line e.epos in
	let rec loop env : value =
		if ctx.debug.breakpoint.bpstate = BPHit && line <> ctx.debug.breakpoint.bpline then ctx.debug.breakpoint.bpstate <- BPEnabled;
		match ctx.debug.debug_state with
			| DbgRunning ->
				run_safe env
			| DbgNext offset ->
				if offset < ctx.environment_offset then
					run_safe env
				else begin
					ctx.debug.debug_state <- DbgWaiting;
					loop env
				end
			| DbgFinish offset ->
				if offset <= ctx.environment_offset then
					run_safe env
				else begin
					ctx.debug.debug_state <- DbgWaiting;
					loop env
				end
			| DbgSingleStep ->
				ctx.debug.debug_state <- DbgWaiting;
				run_safe env
			| DbgWaiting ->
				wait env
	and wait env =
		print_endline (Printf.sprintf "> %s" (s_expr_pretty e));
		match ExtString.String.nsplit (input_line stdin) " " with
			| ["quit" | "exit"] ->
				(* TODO: Borrowed from interp.ml *)
				if (get_ctx()).curapi.use_cache() then raise (Error.Fatal_error ("",Globals.null_pos));
				raise (Interp.Sys_exit 0);
			| ["detach"] ->
				Hashtbl.iter (fun _ h ->
					Hashtbl.clear h
				) ctx.debug.breakpoints;
				ctx.debug.debug_state <- DbgRunning;
				run_safe env
			(* source | history *)
			| ["files" | "filespath"] ->
				Hashtbl.iter (fun i _ ->
					print_endline (rev_hash_s i);
				) ctx.debug.breakpoints;
				loop env
			| ["classes"] ->
				IntMap.iter (fun i _ ->
					print_endline (rev_hash_s i)
				) ctx.type_cache;
				loop env
			| ["mem"] ->
				print_endline (Printf.sprintf "%f" (Gc.stat()).major_words);
				loop env
			| ["compact"] ->
				let before = (Gc.stat()).major_words in
				Gc.compact();
				let after = (Gc.stat()).major_words in
				print_endline (Printf.sprintf "before: %f\nafter: %f" before after);
				loop env
			| ["collect"] ->
				let before = (Gc.stat()).major_words in
				Gc.full_major();
				let after = (Gc.stat()).major_words in
				print_endline (Printf.sprintf "before: %f\nafter: %f" before after);
				loop env
			| ["break" | "b";pattern] ->
				begin try
					let file,line = parse_breakpoint_pattern pattern in
					begin try
						add_breakpoint ctx file line;
						print_endline (Printf.sprintf "Added breakpoint at %s:%i" file line);
					with Not_found ->
						print_endline ("Could not find file " ^ file);
					end;
				with Exit ->
					print_endline ("Unrecognized breakpoint pattern");
				end;
				loop env
			| ["list" | "l"] ->
				(* TODO: other list syntax *)
				iter_breakpoints ctx (fun breakpoint ->
					let flag = match breakpoint.bpstate with
						| BPHit | BPEnabled -> "E"
						| BPDisabled -> "d"
					in
					print_endline (Printf.sprintf "%i %s" breakpoint.bpid flag)
				);
				loop env
			| ["describe" | "desc";bpid] ->
				(* TODO: range patterns? *)
				begin try
					let breakpoint = find_breakpoint ctx bpid in
					print_endline (Printf.sprintf "%s:%i" (rev_hash_s breakpoint.bpfile) breakpoint.bpline);
				with Not_found ->
					print_endline (Printf.sprintf "Unknown breakpoint: %s" bpid);
				end;
				loop env
			| ["disable" | "dis";bpid] ->
				(* TODO: range patterns? *)
				if bpid = "all" then
					iter_breakpoints ctx (fun breakpoint -> breakpoint.bpstate <- BPDisabled)
				else begin try
					let breakpoint = find_breakpoint ctx bpid in
					breakpoint.bpstate <- BPDisabled;
					print_endline (Printf.sprintf "Breakpoint %s disabled" bpid);
				with Not_found ->
					print_endline (Printf.sprintf "Unknown breakpoint: %s" bpid);
				end;
				loop env
			| ["enable" | "en";bpid] ->
				(* TODO: range patterns? *)
				if bpid = "all" then
					iter_breakpoints ctx (fun breakpoint -> breakpoint.bpstate <- BPEnabled)
				else begin try
					let breakpoint = find_breakpoint ctx bpid in
					breakpoint.bpstate <- BPEnabled;
					print_endline (Printf.sprintf "Breakpoint %s enabled" bpid);
				with Not_found ->
					print_endline (Printf.sprintf "Unknown breakpoint: %s" bpid);
				end;
				loop env
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
					print_endline (Printf.sprintf "Breakpoint %s deleted" bpid);
				with Not_found ->
					print_endline (Printf.sprintf "Unknown breakpoint: %s" bpid);
				end;
				loop env
			| ["clear";pattern] ->
				(* TODO: range patterns? *)
				begin try
					let file,line = parse_breakpoint_pattern pattern in
					begin try
						delete_breakpoint ctx file line
					with Not_found ->
						print_endline (Printf.sprintf "Could not find breakpoint %s:%i" file line);
					end
				with Exit ->
					print_endline ("Unrecognized breakpoint pattern");
				end;
				loop env
			(* thread | unsafe | safe *)
			| ["continue" | "c"] ->
				ctx.debug.debug_state <- DbgRunning;
				run_safe env
			| ["step" | "s" | ""] ->
				ctx.debug.debug_state <- DbgSingleStep;
				loop env
			| ["next" | "n"] ->
				ctx.debug.debug_state <- DbgNext ctx.environment_offset;
				f env
			| ["finish" | "f"] ->
				ctx.debug.debug_state <- DbgFinish ctx.environment_offset;
				loop env
			| ["where" | "w"] ->
				print_call_stack ctx e.epos;
				loop env
			(* up | down | frame *)
			| ["variables" | "vars"] ->
				print_locals jit scopes env;
				loop env
			| ["print" | "p";name] ->
				print_variable scopes name env;
				loop env
			| ["set" | "s";name;"=";value] ->
				let msg = ref "" in
				let error s =
					msg := s;
					raise Exit
				in
				begin try
					let e = Parser.parse_expr_string (ctx.curapi.get_com()) value e.epos error false in
					let v = expr_to_value e in
					set_variable scopes name v env
				with Exit ->
					print_endline (Printf.sprintf "Error parsing %s: %s" value !msg);
				end;
				loop env
			| s :: _ ->
				print_endline (Printf.sprintf "Unknown command: %s" s);
				loop env
			| [] ->
				loop env
	and run env =
		try
			f env
		with RunTimeException(v,_,_) when not (is_caught ctx v) ->
			print_endline (uncaught_exception_string v e.epos "");
			ctx.debug.debug_state <- DbgWaiting;
			loop env
	and run_safe env =
		try
			let h = Hashtbl.find ctx.debug.breakpoints jit.file_key in
			let breakpoint = Hashtbl.find h line in
			begin match breakpoint.bpstate with
				| BPEnabled ->
					breakpoint.bpstate <- BPHit;
					ctx.debug.breakpoint <- breakpoint;
					print_endline (Printf.sprintf "Hit breakpoint at %s:%i" (rev_hash_s jit.file_key) line);
					ctx.debug.debug_state <- DbgWaiting;
					loop env
				| _ ->
					run env
			end
		with Not_found ->
			run env
	in
	loop