open Globals
open HxbReaderApi
open HxbReader

class virtual hxb_abstract_reader = object(self)
	inherit hxb_reader_api

	method read_hxb (input : IO.input) =
		let reader = new HxbReader.hxb_reader in
		let result = reader#read (self :> hxb_reader_api) HHDR input in
		let rec loop result = match result with
			| FullModule m ->
				m
			| HeaderOnly(m,cont) ->
				loop (cont (self :> hxb_reader_api) HEND)
		in
		loop result
end