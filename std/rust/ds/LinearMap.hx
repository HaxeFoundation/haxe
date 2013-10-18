package rust.ds;
@:native("hashmap::linear::LinearMap") extern class LinearMap<K, V> {
	public function len():Int;
	public function is_empty():Bool;
	public function clear():Void;
	public function contains_key(k:K):Bool;
	public function each_key(blk:K->Bool):Void;
	public function each_value(blk:V->Bool):Void;
	public function find(k:K):Null<V>;
	public function find_mut(k:K):Null<V>;
	public function insert(k:K, v:V):Bool;
	public function remove(k:K):Bool;
	public function new():Void;
	public function each(f:K -> Bool):Void;
	public function size_hint():Null<Int>;
}