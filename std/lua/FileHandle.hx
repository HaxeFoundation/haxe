package lua;

extern class FileHandle extends UserData {
	public function flush() : Void;
	public function read(arg : haxe.extern.EitherType<String,Int>) : String;
	public function close() : Void;

	@:overload(function () : Int {})
	public function seek(arg : String, pos : Int) : Void;
}

