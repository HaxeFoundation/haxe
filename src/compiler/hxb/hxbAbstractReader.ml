open Globals
open HxbReaderApi

class virtual hxb_abstract_reader = object(self)
	inherit hxb_reader_api

	method read_hxb (input : IO.input) =
		let reader = new HxbReader.hxb_reader in
		reader#read (self :> hxb_reader_api) input
end