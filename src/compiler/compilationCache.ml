open Globals
open Ast
open Json
open Type
open Define

type cached_file = {
	c_file_path : string;
	c_time : float;
	c_package : string list;
	c_decls : type_decl list;
	mutable c_module_name : string option;
	mutable c_pdi : Parser.parser_display_information;
}

type cached_directory = {
	c_path : string;
	mutable c_mtime : float;
}

type cached_native_lib = {
	c_nl_mtime : float;
	c_nl_files : (path,Ast.package) Hashtbl.t;
}

class context_cache (index : int) = object(self)
	val files : (Path.UniqueKey.t,cached_file) Hashtbl.t = Hashtbl.create 0
	val modules : (path,module_def) Hashtbl.t = Hashtbl.create 0
	val removed_files = Hashtbl.create 0
	val mutable json = JNull
	val mutable initialized = false
	val mutable record_changes = false

	val mutable was_initialized = false
	val files_changes : (Path.UniqueKey.t,cached_file option) Hashtbl.t = Hashtbl.create 0
	val removed_files_changes : (Path.UniqueKey.t,string option) Hashtbl.t = Hashtbl.create 0
	val module_changes : (path, module_def option) Hashtbl.t = Hashtbl.create 0
	val module_state_changes : (path, module_cache_state) Hashtbl.t = Hashtbl.create 0

	method prepare () =
		was_initialized <- initialized;
		self#discard_changes ~record:true ();

	method commit () = self#discard_changes ()

	method restore () =
		Hashtbl.iter (fun key file ->
			match file with
			| None -> Hashtbl.remove files key
			| Some file -> Hashtbl.replace files key file
		) files_changes;
		Hashtbl.iter (fun key file ->
			match file with
			| None -> Hashtbl.remove removed_files key
			| Some file -> Hashtbl.replace removed_files key file
		) removed_files_changes;
		Hashtbl.iter (fun p m ->
			match m with
			| None -> Hashtbl.remove modules p
			| Some m -> begin
				Hashtbl.replace modules p m;
				(* Failed attempt at reverting server_exploration task which led me to get recursive inline issues *)
				(* Disabling the task for now... *)
				(* TODO apply to all types after rebase? See https://github.com/HaxeFoundation/haxe/pull/11001 *)
				(* List.iter (fun mt -> match mt with *)
				(* 	| TClassDecl c -> c.cl_restore () *)
				(* 	| _ -> () *)
				(* ) m.m_types *)
			end
		) module_changes;
		Hashtbl.iter (fun p state ->
			try
				let m = Hashtbl.find modules p in
				m.m_extra.m_cache_state <- state
			with Not_found -> ()
		) module_state_changes;
		initialized <- was_initialized;
		self#discard_changes ();
		self

	method discard_changes ?(record=false) () =
		record_changes <- record;
		Hashtbl.reset files_changes;
		Hashtbl.reset removed_files_changes;
		Hashtbl.reset module_changes;
		Hashtbl.reset module_state_changes;

	(* files *)

	method find_file key =
		Hashtbl.find files key

	method cache_file key path time data pdi =
		if record_changes && not (Hashtbl.mem files_changes key) then
			Hashtbl.add files_changes key (Hashtbl.find_opt files key);
		Hashtbl.replace files key { c_file_path = path; c_time = time; c_package = fst data; c_decls = snd data; c_module_name = None; c_pdi = pdi }

	method remove_file key =
		try
			let f = Hashtbl.find files key in
			if record_changes then begin
				if not (Hashtbl.mem files_changes key) then
					Hashtbl.add files_changes key (Hashtbl.find_opt files key);
				if not (Hashtbl.mem removed_files_changes key) then
					Hashtbl.add removed_files_changes key (Hashtbl.find_opt removed_files key);
			end;
			Hashtbl.remove files key;
			Hashtbl.replace removed_files key f.c_file_path
		with Not_found -> ()

	(* Like remove_file, but doesn't keep track of the file *)
	method remove_file_for_real key =
		(* Below lines actually break the initial fix if uncommented *)
		(* if record_changes && not (Hashtbl.mem files_changes key) then *)
		(* 	Hashtbl.add files_changes key (Hashtbl.find_opt files key); *)
		Hashtbl.remove files key

	method replace_removed_file key file =
		if record_changes && not (Hashtbl.mem removed_files_changes key) then
			Hashtbl.add removed_files_changes key (Hashtbl.find_opt removed_files key);
		Hashtbl.replace removed_files key file

	method remove_removed_file key =
		if record_changes && not (Hashtbl.mem removed_files_changes key) then
			Hashtbl.add removed_files_changes key (Hashtbl.find_opt removed_files key);
		Hashtbl.remove removed_files key

	(* modules *)

	method find_module path =
		Hashtbl.find modules path

	method find_module_opt path =
		Hashtbl.find_opt modules path

	method cache_module path value =
		if record_changes && not (Hashtbl.mem module_changes path) then
			Hashtbl.add module_changes path (Hashtbl.find_opt modules path);
		Hashtbl.replace modules path value

	method taint_module path m reason =
		if record_changes && not (Hashtbl.mem module_changes path) && not (Hashtbl.mem module_state_changes path) then
			Hashtbl.add module_state_changes path m.m_extra.m_cache_state;
		m.m_extra.m_cache_state <- MSBad (Tainted reason)

	(* initialization *)

	method is_initialized = initialized
	method set_initialized value = initialized <- value

	method get_index = index
	method get_files = files
	method get_modules = modules
	method get_removed_files = removed_files

	method get_json = json
	method set_json j = json <- j

(* Pointers for memory inspection. *)
	method get_pointers : unit array =
		[|Obj.magic files;Obj.magic modules|]
end

let create_directory path mtime = {
	c_path = path;
	c_mtime = mtime;
}

class virtual server_task (id : string list) (priority : int) = object(self)
	method private virtual execute : unit

	method run : unit =
		let t = Timer.timer ("server" :: "task" :: id) in
		Std.finally t (fun () -> self#execute) ()

	method get_priority = priority
	method get_id = id
end

class arbitrary_task (id : string list) (priority : int) (f : unit -> unit) = object(self)
	inherit server_task id priority

	method private execute =
		f ()
end

class cache = object(self)
	val contexts : (string,context_cache) Hashtbl.t = Hashtbl.create 0
	val mutable context_list = []
	val haxelib : (string list, string list) Hashtbl.t = Hashtbl.create 0
	val directories : (string, cached_directory list) Hashtbl.t = Hashtbl.create 0
	val native_libs : (string,cached_native_lib) Hashtbl.t = Hashtbl.create 0
	val mutable tasks : (server_task PriorityQueue.t) = PriorityQueue.Empty

	val mutable record_changes = false
	val mutable added_contexts = []
	val haxelib_changes : (string list, string list option) Hashtbl.t = Hashtbl.create 0
	val native_lib_changes : (string, cached_native_lib option) Hashtbl.t = Hashtbl.create 0
	val directory_changes : (string, cached_directory list option) Hashtbl.t = Hashtbl.create 0

	(* TODO FIXME *)
	(* Note: running restore at the end (well, was it really?) of the request was not *)
	(* fixing the original issue. This seems to work better but ehhh that's ugly and *)
	(* I don't have much confidence in it. *)
	method prepare () =
		self#restore ();
		record_changes <- true;

	method commit () =
		self#discard_changes ();
		List.iter (fun c -> c#commit ()) context_list;

	method restore () =
		List.iter (Hashtbl.remove contexts) added_contexts;
		context_list <- Hashtbl.fold (fun _ ctx acc -> ctx#restore () :: acc) contexts [];
		Hashtbl.iter (fun key value ->
			match value with
			| None -> Hashtbl.remove haxelib key
			| Some value -> Hashtbl.replace haxelib key value
		) haxelib_changes;
		Hashtbl.iter (fun key value ->
			match value with
			| None -> Hashtbl.remove native_libs key
			| Some value -> Hashtbl.replace native_libs key value
		) native_lib_changes;
		Hashtbl.iter (fun key value ->
			match value with
			| None -> Hashtbl.remove directories key
			| Some value -> Hashtbl.replace directories key value
		) directory_changes;
		self#discard_changes ();

	method discard_changes () =
		record_changes <- false;
		added_contexts <- [];
		Hashtbl.reset haxelib_changes;
		Hashtbl.reset native_lib_changes;
		Hashtbl.reset directory_changes;

	(* contexts *)

	method get_context sign =
		try
			Hashtbl.find contexts sign
		with Not_found ->
			let cache = new context_cache (Hashtbl.length contexts) in
			context_list <- cache :: context_list;
			added_contexts <- sign :: added_contexts;
			Hashtbl.add contexts sign cache;
			cache

	method add_info sign desc platform class_path defines =
		let cc = self#get_context sign in
		let jo = JObject [
			"index",JInt cc#get_index;
			"desc",JString desc;
			"platform",JString (platform_name platform);
			"classPaths",JArray (List.map (fun s -> JString s) class_path);
			"signature",JString (Digest.to_hex sign);
			"defines",JArray (PMap.foldi (fun k v acc -> JObject [
				"key",JString k;
				"value",JString v;
			] :: acc) defines.values []);
		] in
		cc#set_json jo;
		cc#get_index

	method get_contexts = context_list

	(* files *)

	method remove_files file =
		Hashtbl.iter (fun _ cc-> cc#remove_file file) contexts

	method get_context_files signs =
		Hashtbl.fold (fun sign cc acc ->
			if List.mem sign signs then Hashtbl.fold (fun file cfile acc -> (file,cfile) :: acc) cc#get_files acc
			else acc
		) contexts []

	method get_files =
		Hashtbl.fold (fun sign cc acc ->
			Hashtbl.fold (fun file cfile acc -> (sign,file,cfile) :: acc) cc#get_files acc
		) contexts []

	(* modules *)

	method iter_modules f =
		Hashtbl.iter (fun _ cc ->
			Hashtbl.iter (fun _ m ->
				f m
			) cc#get_modules
		) contexts

	method get_modules =
		Hashtbl.fold (fun _ cc acc ->
			Hashtbl.fold (fun _ m acc ->
				m :: acc
			) cc#get_modules acc
		) contexts []

	method taint_modules file_key reason =
		Hashtbl.iter (fun _ cc ->
			Hashtbl.iter (fun path m ->
				if Path.UniqueKey.lazy_key m.m_extra.m_file = file_key then cc#taint_module path m reason
			) cc#get_modules
		) contexts

	(* haxelibs *)

	method find_haxelib key =
		Hashtbl.find haxelib key

	method cache_haxelib key value =
		if record_changes && not (Hashtbl.mem haxelib_changes key) then
				Hashtbl.add haxelib_changes key (Hashtbl.find_opt haxelib key);
		Hashtbl.replace haxelib key value

	(* directories *)

	method find_directories key =
		Hashtbl.find directories key

	method add_directories key value =
		if record_changes && not (Hashtbl.mem directory_changes key) then
				Hashtbl.add directory_changes key (Hashtbl.find_opt directories key);
		Hashtbl.replace directories key value

	method remove_directory key value =
		if record_changes && not (Hashtbl.mem directory_changes key) then
				Hashtbl.add directory_changes key (Hashtbl.find_opt directories key);
		try
			let current = self#find_directories key in
			Hashtbl.replace directories key (List.filter (fun dir -> dir.c_path <> value) current);
		with Not_found ->
			()

	method has_directory key value =
		try
			List.exists (fun dir -> dir.c_path = value) (self#find_directories key)
		with Not_found ->
			false

	method add_directory key value =
		try
			let current = self#find_directories key in
			self#add_directories key (value :: current)
		with Not_found ->
			self#add_directories key [value]

	method clear_directories key =
		if record_changes && not (Hashtbl.mem directory_changes key) then
				Hashtbl.add directory_changes key (Hashtbl.find_opt directories key);
		Hashtbl.remove directories key

	(* native lib *)

	method add_native_lib key files timestamp =
		if record_changes && not (Hashtbl.mem native_lib_changes key) then
				Hashtbl.add native_lib_changes key (Hashtbl.find_opt native_libs key);
		Hashtbl.replace native_libs key { c_nl_files = files; c_nl_mtime = timestamp }

	method get_native_lib key =
		try Some (Hashtbl.find native_libs key)
		with Not_found -> None

	(* tasks *)

	method add_task (task : server_task) : unit =
		tasks <- PriorityQueue.insert tasks task#get_priority task

	method has_task =
		not (PriorityQueue.is_empty tasks)

	method get_task =
		let (_,task,queue) = PriorityQueue.extract tasks in
		tasks <- queue;
		task

	method run_tasks recursive f =
		let rec loop acc =
			let current = tasks in
			tasks <- Empty;
			let f (ran_task,acc) prio task =
				if f task then begin
					task#run;
					(true,acc)
				end else
					ran_task,PriorityQueue.insert acc prio task
			in
			let ran_task,folded = PriorityQueue.fold current f (false,acc) in
			if recursive && ran_task then loop folded
			else folded
		in
		tasks <- PriorityQueue.merge tasks (loop PriorityQueue.Empty);

	(* Pointers for memory inspection. *)
	method get_pointers : unit array =
		[|Obj.magic contexts;Obj.magic haxelib;Obj.magic directories;Obj.magic native_libs|]

end

type t = cache

type context_options =
	| NormalContext
	| MacroContext
	| NormalAndMacroContext

let get_module_name_of_cfile file cfile = match cfile.c_module_name with
	| None ->
		let name = Path.module_name_of_file file in
		cfile.c_module_name <- Some name;
		name
	| Some name ->
		name
