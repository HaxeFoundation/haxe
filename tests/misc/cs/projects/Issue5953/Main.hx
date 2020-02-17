class Main {
	static function main() {
		var map = foo(null);
		trace(map);
	}

	static function foo(m:NativeStringMap<String>)
		return m.toMap();
}

abstract NativeStringMap<V>(Impl<V>) {
	@:to public function toMap():Map<String, V> {
		return new Map();
	}
}

typedef Impl<V> = cs.system.collections.generic.IDictionary_2<String, V>;
