
prerr_endline (Extc.executable_path());
let contents = Std.input_file "test.ml" in
let s = Extc.unzip (Extc.zip contents) in
if s <> contents then failwith "zip + unzip failed";
