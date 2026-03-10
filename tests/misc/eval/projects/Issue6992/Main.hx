class Main {
	static public function main() {
		foo(12);
	}

	inline static function foo( b ) { foo(b); }
}