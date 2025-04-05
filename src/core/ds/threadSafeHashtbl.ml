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