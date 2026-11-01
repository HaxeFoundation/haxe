open Globals

type t = {
	mutable restricted_suspension : bool;
}

let create () = {
	restricted_suspension = false;
}

module CoroScopeConfigReader (API : DataReaderApi.DataReaderApi) = struct
	let read_coro_scope_config config data =
		let read data =
			let fl = API.read_object data in
			List.iter (fun (s, data) -> match s with
				| "restrictedSuspension" ->
					config.restricted_suspension <- API.read_bool data
				| s ->
					Error.raise_typing_error (Printf.sprintf "Unknown key for coroutine scope config: %s" s) null_pos
			) fl
		in
		API.read_optional data read
end

module CoroScopeConfigReaderMeta = CoroScopeConfigReader(MetaDataApi.MetaReaderApi)

(**
	Returns the coroutine scope config for a given metadata list, or `None` if
	the type is not a coroutine scope.
*)
let of_meta_list meta =
	match Meta.get Meta.CoroutineScope meta with
	| entry ->
		let config = create () in
		CoroScopeConfigReaderMeta.read_coro_scope_config config (MetaDataApi.of_metadata_entry entry);
		Some config
	| exception Not_found ->
		None
