
class virtual ['key,'value] lookup = object(self)
	method virtual add : 'key -> 'value -> unit
	method virtual remove : 'key -> unit
	method virtual find : 'key -> 'value
	method virtual iter : ('key -> 'value -> unit) -> unit
	method virtual fold : 'acc . ('key -> 'value -> 'acc -> 'acc) -> 'acc -> 'acc
	method virtual mem : 'key -> bool
	method virtual clear : unit

	method virtual start_group : int
	method virtual commit_group : int -> int
	method virtual discard_group : int -> int
end

class ['key,'value] pmap_lookup = object(self)
	inherit ['key,'value] lookup
	val mutable lut : ('key,'value) PMap.t = PMap.empty

	val mutable group_id : int ref = ref 0
	val mutable groups : (int,'key list) PMap.t = PMap.empty

	method add (key : 'key) (value : 'value) =
		groups <- PMap.map (fun modules -> key :: modules) groups;
		lut <- PMap.add key value lut

	method remove (key : 'key) =
		lut <- PMap.remove key lut

	method find (key : 'key) : 'value =
		PMap.find key lut

	method iter (f : 'key -> 'value -> unit) =
		PMap.iter f lut

	method fold : 'acc . ('key -> 'value -> 'acc -> 'acc) -> 'acc -> 'acc = fun f acc ->
		PMap.foldi f lut acc

	method mem (key : 'key) =
		PMap.mem key lut

	method clear =
		lut <- PMap.empty

	method start_group =
		incr group_id;
		let i = !group_id in
		groups <- PMap.add i [] groups;
		i

	method commit_group i =
		let group = PMap.find i groups in
		let n = List.length group in
		groups <- PMap.remove i groups;
		n

	method discard_group i =
		let group = PMap.find i groups in
		let n = List.length group in
		List.iter (fun mpath -> self#remove mpath) group;
		groups <- PMap.remove i groups;
		n
end

class ['key,'value] hashtbl_lookup = object(self)
	inherit ['key,'value] lookup
	val lut : ('key,'value) Hashtbl.t = Hashtbl.create 0

	val mutable group_id : int ref = ref 0
	val mutable groups : (int,'key list) Hashtbl.t = Hashtbl.create 0

	method add (key : 'key) (value : 'value) =
		Hashtbl.iter (fun i modules -> Hashtbl.replace groups i (key :: modules)) groups;
		Hashtbl.replace lut key value

	method remove (key : 'key) =
		Hashtbl.remove lut key

	method find (key : 'key) : 'value =
		Hashtbl.find lut key

	method iter (f : 'key -> 'value -> unit) =
		Hashtbl.iter f lut

	method fold : 'acc . ('key -> 'value -> 'acc -> 'acc) -> 'acc -> 'acc = fun f acc ->
		Hashtbl.fold f lut acc

	method mem (key : 'key) =
		Hashtbl.mem lut key

	method clear =
		Hashtbl.clear lut

	method start_group =
		incr group_id;
		let i = !group_id in
		Hashtbl.replace groups i [];
		i

	method commit_group i =
		let group = Hashtbl.find groups i in
		let n = List.length group in
		Hashtbl.remove groups i;
		n

	method discard_group i =
		let group = Hashtbl.find groups i in
		let n = List.length group in
		List.iter (fun mpath -> self#remove mpath) group;
		Hashtbl.remove groups i;
		n
end

