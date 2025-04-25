type ('a,'b) t = {
	h : ('a,'b) Hashtbl.t;
	mutex : Mutex.t
}

let create size = {
	h = Hashtbl.create size;
	mutex = Mutex.create ();
}

let add h k v =
	Mutex.protect h.mutex (fun () -> Hashtbl.add h.h k) v

let replace h k v =
	Mutex.protect h.mutex (fun () -> Hashtbl.replace h.h k) v

let find h k =
	Mutex.protect h.mutex (fun () -> Hashtbl.find h.h) k

let find_or_add h k f =
	Mutex.lock h.mutex;
	try
		let r = Hashtbl.find h.h k in
		Mutex.unlock h.mutex;
		r
	with Not_found ->
		let r = f () in
		Hashtbl.add h.h k r;
		Mutex.unlock h.mutex;
		r

let mem h k =
	Mutex.protect h.mutex (fun () -> Hashtbl.mem h.h) k

let remove h k =
	Mutex.protect h.mutex (fun () -> Hashtbl.remove h.h) k

let fold f h acc =
	Mutex.protect h.mutex (fun () -> Hashtbl.fold f h.h) acc