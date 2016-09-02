package flash.trace;

extern class Trace {
	static var FILE(default,never) : Dynamic;
	static var LISTENER(default,never) : Dynamic;
	static var METHODS(default,never) : Int;
	static var METHODS_AND_LINES(default,never) : Int;
	static var METHODS_AND_LINES_WITH_ARGS(default,never) : Int;
	static var METHODS_WITH_ARGS(default,never) : Int;
	static var OFF(default,never) : Int;
	static function getLevel(target : Int = 2) : Int;
	static function getListener() : Dynamic;
	static function setLevel(l : Int, target : Int = 2) : Dynamic;
	static function setListener(f : Dynamic) : Dynamic;
}
