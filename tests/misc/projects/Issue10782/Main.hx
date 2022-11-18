@:callable
abstract Function(Int->Int) to Int->Int from Int->Int {
	@:op(a + b)
	static function add(a:Function, b:Function):Function {
		return function(i) {
			return a(b(i));
		};
	}
}

class Main {
	static function main() {
		var f:Function = function(a) {
			return a + 1;
		}

		var g = function(a) { return a * a; } + f;
        if (g(10) != 121) {
            throw "Incorrect function return value";
        }
	}
}
