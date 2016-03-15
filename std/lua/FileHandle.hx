package lua;

extern class FileHandle extends UserData {
	public function flush() : Void;
	public function read(arg : haxe.extern.EitherType<ReadOption,Int>) : String;
	public function close() : Void;

	@:overload(function () : Int {})
	public function seek(arg : String, pos : Int) : Void;
}

@:enum
abstract ReadOption(String) {
	var All = "*all";
	var Line = "*line";
	var Number = "*number";
}
