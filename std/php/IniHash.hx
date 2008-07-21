package php;

class IniHash extends Hash<String> {
	public function new(file : String) {
		super();
		if(file == null) throw "File can't be null";
		h = untyped __call__("parse_ini_file", file, false);
	}
}