package java.util;

extern interface Map<K,V>
{
	function clear():Void;
	function containsKey(obj:Dynamic):Bool;
	function containsValue(obj:Dynamic):Bool;
	function entrySet():java.util.Set<MapEntry<K,V>>;
	function get(k:Dynamic):V;
	function keySet():java.util.Set<K>;
	function put(key:K, value:V):V;
	function remove(key:Dynamic):V;
	function size():Int;
	function values():Collection<V>;
}

@:native('java.util.Map.Entry') extern interface MapEntry<K,V>
{
	function getKey():K;
	function getValue():V;
	function setValue(v:V):V;
}