package js.es6;
#if harmony
@:native("Map") extern class Map<K, V> {
	function new():Void;
	function get(key:K):V;
	function has(key:K):Bool;
	function set(key:K, val:V):Void;
	function delete(key:K):Bool;
	function values():Array<V>;
	function keys():Array<K>;
}
#else
#error "Must be targetting JS Harmony"
#end