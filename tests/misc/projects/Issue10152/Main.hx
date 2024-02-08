class Main {
	static function main () {
	#if (foo != "bar")
		trace("NOT EQUAL");
	#else
		trace("EQUAL");
	#end
	}
}