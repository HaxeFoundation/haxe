open Globals
open Ast
open Json
open Type
open Define

type cached_file = {
	c_file_path : ClassPaths.resolved_file;
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

let get_module_name_of_cfile file cfile = match cfile.c_module_name with
	| None ->
		let name = Path.module_name_of_file file in
		cfile.c_module_name <- Some name;
		name
	| Some name ->
		name

class context_cache (index : int) (sign : Digest.t) = object(self)
	val files : (Path.UniqueKey.t,cached_file) Hashtbl.t = Hashtbl.create 0
	val modules : (path,module_def) Hashtbl.t = Hashtbl.create 0
	val binary_cache : (path,HxbData.module_cache) Hashtbl.t = Hashtbl.create 0
	val tmp_binary_cache : (path,HxbData.module_cache) Hashtbl.t = Hashtbl.create 0
	val string_pool  = StringPool.create ()
	val removed_files = Hashtbl.create 0
	val mutable json = JNull
	val mutable initialized = false

	(* files *)

	method find_file key =
		Hashtbl.find files key

	method cache_file key path time data pdi =
		Hashtbl.replace files key { c_file_path = path; c_time = time; c_package = fst data; c_decls = snd data; c_module_name = None; c_pdi = pdi }

	method remove_file key =
		try
			let f = Hashtbl.find files key in
			Hashtbl.remove files key;
			Hashtbl.replace removed_files key f.c_file_path
		with Not_found -> ()

	(* Like remove_file, but doesn't keep track of the file *)
	method remove_file_for_real key =
		Hashtbl.remove files key

	(* modules *)

	method find_module path =
		Hashtbl.find modules path

	method find_module_opt path =
		Hashtbl.find_opt modules path

	method get_hxb_module path =
		try Hashtbl.find tmp_binary_cache path
		with Not_found ->
			let mc = Hashtbl.find binary_cache path in
			let m_extra = { mc.mc_extra with m_deps = mc.mc_extra.m_deps } in
			let mc = { mc with mc_extra = m_extra } in
			Hashtbl.add tmp_binary_cache path mc;
			mc

	method find_module_extra path =
		try (Hashtbl.find modules path).m_extra
		with Not_found -> (self#get_hxb_module path).mc_extra

	method cache_hxb_module config warn anon_identification path m =
		match m.m_extra.m_kind with
		| MImport ->
			Hashtbl.add modules m.m_path m
		| _ ->
			let writer = HxbWriter.create config (Some string_pool) warn anon_identification in
			HxbWriter.write_module writer m;

			(* TODO: avoid running the whole writer again... *)
			let anon_identification = new Tanon_identification.tanon_identification in
			let min_writer = HxbWriter.create config None warn anon_identification in
			min_writer.minimal <- true;
			HxbWriter.write_module min_writer m;

			Hashtbl.replace binary_cache path {
				mc_path = path;
				mc_id = m.m_id;
				mc_chunks = HxbWriter.get_chunks writer;
				mc_min_chunks = HxbWriter.get_chunks min_writer;
				mc_extra = { m.m_extra with
					m_cache_state = MSGood;
					m_sig_deps = Some (HxbWriter.get_dependencies min_writer)
				}
			}

	method cache_module_in_memory path m =
		Hashtbl.replace modules path m

	method clear_temp_cache =
		Hashtbl.clear tmp_binary_cache

	method clear_cache =
		Hashtbl.clear modules;
		self#clear_temp_cache

	(* initialization *)

	method is_initialized = initialized
	method set_initialized value = initialized <- value

	method get_sign = sign
	method get_index = index
	method get_files = files
	method get_modules = modules

	method get_hxb = binary_cache
	method get_string_pool = string_pool
	method get_string_pool_arr = string_pool.items.arr

	(* TODO handle hxb cache there too *)
	method get_removed_files = removed_files

	method get_json = json
	method set_json j = json <- j

(* Pointers for memory inspection. *)
	method get_pointers : unit array =
		[|Obj.magic files;Obj.magic modules;Obj.magic binary_cache|]
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

	method clear =
		Hashtbl.clear contexts;
		context_list <- [];
		Hashtbl.clear haxelib;
		Hashtbl.clear directories;
		Hashtbl.clear native_libs;
		tasks <- PriorityQueue.Empty

	(* contexts *)

	method clear_temp_cache =
		Hashtbl.iter (fun _ ctx -> ctx#clear_temp_cache) contexts

	method get_context sign =
		try
			Hashtbl.find contexts sign
		with Not_found ->
			let cache = new context_cache (Hashtbl.length contexts) sign in
			context_list <- cache :: context_list;
			Hashtbl.add contexts sign cache;
			cache

	method add_info sign desc platform (class_paths : ClassPaths.class_paths) defines =
		let cc = self#get_context sign in
		let jo = JObject [
			"index",JInt cc#get_index;
			"desc",JString desc;
			"platform",JString (platform_name platform);
			"classPaths",JArray (List.map (fun s -> JString s) class_paths#as_string_list);
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
			Hashtbl.iter (fun _ m ->
				if Path.UniqueKey.lazy_key m.m_extra.m_file = file_key then m.m_extra.m_cache_state <- MSBad (Tainted reason)
			) cc#get_modules;
			let open HxbData in
			Hashtbl.iter (fun _ mc ->
				if Path.UniqueKey.lazy_key mc.mc_extra.m_file = file_key then
					mc.mc_extra.m_cache_state <- match reason, mc.mc_extra.m_cache_state with
					| CheckDisplayFile, (MSBad _ as state) -> state
					| _ -> MSBad (Tainted reason)
			) cc#get_hxb
		) contexts

	(* haxelibs *)

	method find_haxelib key =
		Hashtbl.find haxelib key

	method cache_haxelib key value =
		Hashtbl.replace haxelib key value

	(* directories *)

	method find_directories key =
		Hashtbl.find directories key

	method add_directories key value =
		Hashtbl.replace directories key value

	method remove_directory key value =
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
		Hashtbl.remove directories key

	(* native lib *)

	method add_native_lib key files timestamp =
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
