open Globals
open HxbReaderApi

class virtual hxb_abstract_reader (p : pos) = object(self)
	inherit hxb_reader_api

	method read_hxb (input : IO.input) =
		let reader = new HxbReader.hxb_reader (self :> hxb_reader_api) in
		reader#read input true p
end