import pack.Sub;

class Main {
	static public var test:Int;

	static function main() {
		test = Sub.test;
		Sub.test = test;
	}
}