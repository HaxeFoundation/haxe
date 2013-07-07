package rust.ds;
@:native("std::treemap::TreeMap")
extern class TreeMap<K, V> {
	public function clear();
	public function contains_key(key:K):Bool;
	public function each_key(fn:K->Bool):Void;
	public function each_value(fn:V->Bool)
}