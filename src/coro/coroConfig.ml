open Globals

type t = {
	mutable debug : bool;
	mutable nothrow : bool;
}

let create () = {
	debug = false;
	nothrow = false;
}

module CoroConfigReader (API : DataReaderApi.DataReaderApi) = struct
	let read_coro_config config data =
		let read data =
			let fl = API.read_object data in
			List.iter (fun (s, data) -> match s with
				| "debug" ->
					config.debug <- API.read_bool data
				| "nothrow" ->
					config.nothrow <- API.read_bool data
				| s ->
					Error.raise_typing_error (Printf.sprintf "Unknown key for coroutine config: %s" s) null_pos
			) fl
		in
		API.read_optional data read
end

module CoroConfigReaderMeta = CoroConfigReader(MetaDataApi.MetaReaderApi)

let of_metadata_entry entry =
	let config = create () in
	CoroConfigReaderMeta.read_coro_config config (MetaDataApi.of_metadata_entry entry);
	config
