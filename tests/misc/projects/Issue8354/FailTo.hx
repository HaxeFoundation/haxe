class FailTo {
	static function main() {}
}

class MyMap<K, V> {
	var values: Array<V>;
	public function iterator(): Iterator<V> return values.iterator();
}

abstract AbstractMap<K,V>(MyMap<K,V>) to Iterable<K> {}