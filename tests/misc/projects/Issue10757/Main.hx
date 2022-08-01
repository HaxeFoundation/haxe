enum Foo {
	A(s: String); B(i: Int);
  }

  class Main {
	static function main() {
	  var f1 = A("");
	  var f2 = B(0);
	  // var m = [1 => f1, 2 => f2];
	  trace(f1 == f2);
	}
  }