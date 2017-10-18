package js;

@:native("Map")
extern class Map<K,V> {
	var size(default,null):Int;
	function new();
	function has(key:K):Bool;
	function get(key:K):Null<V>;
	function set(key:K, value:V):Map<K,V>;
	function delete(key:K):Bool;
	function clear():Void;
	function forEach(callback:(value:V, key:K, map:Map<K,V>)->Void, ?thisArg:Any):Void;
}
