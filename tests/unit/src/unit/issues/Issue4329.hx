package unit.issues;

class Issue4329 extends Test {

	static function foo(x:Int) {
		var r = switch( x ) {
		case 5: 3;
		default:
			return bar();
		}
	}

	static function bar() { }

	function test() {
		foo(12);
	}
}