class Main1 {
	static function id<T>(x:T):T return x;

	static function main() {
		id<String>("s");
	}
}
