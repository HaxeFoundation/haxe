class Main {
	static function main() {
		(1:A1<String>)['foo'];
		(1:A2<Int>)['bar'];
		(1:A2<Main>)['baz'];
	}
}

abstract A1<T>(Int) from Int {
	@:op([]) static function get<S>(instance: A1<S>, key: String): String {
		return '${instance}${key}';
	}
}

abstract A2<T>(Int) from Int {
	@:op([]) static function get(instance: A2<Int>, key: String): String {
		return '${instance}${key}';
	}
}