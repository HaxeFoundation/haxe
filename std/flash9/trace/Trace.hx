package flash.trace;

extern class Trace {
	static var FILE : Int;
	static var LISTENER : Int;
	static var METHODS : Int;
	static var METHODS_AND_LINES : Int;
	static var METHODS_AND_LINES_WITH_ARGS : Int;
	static var METHODS_WITH_ARGS : Int;
	static var OFF : Int;
	static function getLevel(?target : Int) : Int;
	static function getListener() : Dynamic;
	static function setLevel(l : Int, ?target : Int) : Dynamic;
	static function setListener(f : Dynamic) : Dynamic;
}
