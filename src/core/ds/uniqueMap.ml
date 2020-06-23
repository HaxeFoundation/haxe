include PMap

let maybe_add k v l =
	if not (PMap.mem k l) then
		PMap.add k v l
	else
		l

let keys l =
	PMap.foldi (fun k _ acc -> k :: acc) l []

let values l =
	PMap.fold (fun v acc -> v :: acc) l []