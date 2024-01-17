class virtual any_output = object(self)
	method virtual add_entry : string -> string -> unit
	method virtual close : unit
end

class zip_output
	(zip_path : string)
	(compression_level : int)
= object(self)
	inherit any_output
	val zip = Zip.open_out zip_path

	method add_entry (content : string) (name : string) =
		Zip.add_entry ~level:compression_level content zip name

	method close =
		Zip.close_out zip
end