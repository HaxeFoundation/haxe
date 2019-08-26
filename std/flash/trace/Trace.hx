package flash.trace;

extern class Trace {
	static final FILE : Dynamic;
	static final LISTENER : Dynamic;
	static final METHODS : Int;
	static final METHODS_AND_LINES : Int;
	static final METHODS_AND_LINES_WITH_ARGS : Int;
	static final METHODS_WITH_ARGS : Int;
	static final OFF : Int;
	static function getLevel(target : Int = 2) : Int;
	static function getListener() : Dynamic;
	static function setLevel(l : Int, target : Int = 2) : Dynamic;
	static function setListener(f : Dynamic) : Dynamic;
}
