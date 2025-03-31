
class virtual ['key,'value] lookup = object(self)
	method virtual add : 'key -> 'value -> unit
	method virtual remove : 'key -> unit
	method virtual find : 'key -> 'value
	method virtual iter : ('key -> 'value -> unit) -> unit
	method virtual fold : 'acc . ('key -> 'value -> 'acc -> 'acc) -> 'acc -> 'acc
	method virtual mem : 'key -> bool
	method virtual clear : unit
end

class ['key,'value] pmap_lookup = object(self)
	inherit ['key,'value] lookup
	val mutable lut : ('key,'value) PMap.t = PMap.empty

	method add (key : 'key) (value : 'value) =
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
end

class ['key,'value] hashtbl_lookup = object(self)
	inherit ['key,'value] lookup
	val lut : ('key,'value) Hashtbl.t = Hashtbl.create 0

	method add (key : 'key) (value : 'value) =
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
end

