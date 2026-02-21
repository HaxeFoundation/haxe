open Globals

type coro_assert = {
	mutable num_states : int option;
}

type t = {
	mutable debug : bool;
	mutable nothrow : bool;
	mutable assert_config : coro_assert option;
}

let create () = {
	debug = false;
	nothrow = false;
	assert_config = None;
}

module CoroConfigReader (API : DataReaderApi.DataReaderApi) = struct
	let read_coro_assert config_assert data =
		let fl = API.read_object data in
		List.iter (fun (s, data) -> match s with
			| "numStates" ->
				config_assert.num_states <- Some (API.read_int data)
			| s ->
				Error.raise_typing_error (Printf.sprintf "Unknown key for coroutine assert config: %s" s) null_pos
		) fl

	let read_coro_config config data =
		let read data =
			let fl = API.read_object data in
			List.iter (fun (s, data) -> match s with
				| "debug" ->
					config.debug <- API.read_bool data
				| "nothrow" ->
					config.nothrow <- API.read_bool data
				| "assert" ->
					let config_assert = { num_states = None } in
					read_coro_assert config_assert data;
					config.assert_config <- Some config_assert
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
