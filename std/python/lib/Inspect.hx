package python.lib;

import python.lib.Tuple.Tup2;

@:pythonImport("inspect")
extern class Inspect {
	static function getmembers(object:Dynamic, ?predicate:Dynamic->Bool):Array<Tup2<String,Dynamic>>;
	static function ismethod(object:Dynamic):Bool;
	static function isclass(object:Dynamic):Bool;
	static function isfunction(object:Dynamic):Bool;
}
