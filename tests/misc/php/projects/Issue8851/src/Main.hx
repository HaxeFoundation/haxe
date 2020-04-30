class Main {
	static var tmp:Any;

	static function main() {
		iter([1 => 2]);
		iter(['1' => 2]);
		iter([{field:1} => 2]);
		iter([1, 2]);
	}

	static function iter<T>(it:Iterable<T>) {
		for(i in it) {
			tmp = i;
		}
	}
}