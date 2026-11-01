class Main {
	static function main() {
		#if FOO.BAR
		// pass
		#else
		throw 'FOO.BAR condition failed';
		#end

		#if foo.bar
		// pass
		#else
		throw 'foo.bar condition failed';
		#end
	}
}
