type 'a t = {
	mutable value: 'a option;
	mutex: Mutex.t;
	compute: unit->'a
}

let from_fun f =
	{ value = None; mutex = Mutex.create (); compute = (fun () -> f()) }

let force lazy_val =
	match lazy_val.value with
		| None ->
			Mutex.protect lazy_val.mutex (fun () ->
				match lazy_val.value with
					| None ->
						let result = lazy_val.compute () in
						lazy_val.value <- Some result;
						result
					| Some result -> result
			)
		| Some v -> v

