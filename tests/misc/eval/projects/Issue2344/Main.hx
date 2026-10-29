abstract MyInt(Int) from Int {
	@:op(A + B) public static function add(a:MyInt, b:MyInt):MyInt;
}

class Main {
	static function main() {
		var a:MyInt = 1;
		var b:MyInt = 2;
		MyInt.add(a, b);
	}
}
