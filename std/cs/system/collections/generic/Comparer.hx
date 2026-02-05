package cs.system.collections.generic;

@:native("System.Collections.Generic.Comparer")
extern class Comparer<T> {
	static var Default(default, never):cs.system.collections.generic.Comparer<Dynamic>;
	static function Create<T>(comparison:cs.system.Comparison<T>):cs.system.collections.generic.Comparer<T>;
	function Compare(x:T, y:T):Int;
}
