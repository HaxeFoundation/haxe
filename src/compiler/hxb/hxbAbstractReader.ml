open HxbReaderApi
open HxbData

class virtual hxb_abstract_reader = object(self)
	inherit hxb_reader_api

	method read_hxb (input : IO.input) (stats : HxbReader.hxb_reader_stats) =
		let reader = new HxbReader.hxb_reader stats in
		reader#read (self :> hxb_reader_api) input

	method read_chunks (chunks : cached_chunks) (stats : HxbReader.hxb_reader_stats) =
		fst (self#read_chunks_until chunks stats EOM)

	method read_chunks_until (chunks : cached_chunks) (stats : HxbReader.hxb_reader_stats) (until : HxbData.chunk_kind) =
		let reader = new HxbReader.hxb_reader stats in
		reader#read_chunks_until (self :> hxb_reader_api) chunks until
end
