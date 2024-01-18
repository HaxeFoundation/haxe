open HxbReaderApi
open HxbData

class virtual hxb_abstract_reader = object(self)
	inherit hxb_reader_api

	method read_hxb (path : Globals.path) (bytes : bytes) (stats : HxbReader.hxb_reader_stats) =
		let reader = new HxbReader.hxb_reader path stats in
		reader#read (self :> hxb_reader_api) bytes

	method read_chunks (path : Globals.path) (chunks : cached_chunks) (stats : HxbReader.hxb_reader_stats) =
		fst (self#read_chunks_until path chunks stats EOM)

	method read_chunks_until (path : Globals.path) (chunks : cached_chunks) (stats : HxbReader.hxb_reader_stats) (until : HxbData.chunk_kind) =
		let reader = new HxbReader.hxb_reader path stats in
		reader#read_chunks_until (self :> hxb_reader_api) chunks until
end
