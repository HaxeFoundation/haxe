open HxbReaderApi
open HxbReader

class virtual hxb_abstract_reader = object(self)
	inherit hxb_reader_api

	method read_hxb (input : IO.input) (stats : HxbReader.hxb_reader_stats) =
		let reader = new HxbReader.hxb_reader stats in
		reader#read (self :> hxb_reader_api) input
end
