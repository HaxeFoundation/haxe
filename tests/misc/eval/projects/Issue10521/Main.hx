class Main {
	static function main() {
		new Bar(1, 2);
	}
}

class Foo {
	var x:Int;
	var y:Int;

	public function new(x:Int, y:Int) {
		this.x = x;
		this.y = y;
	}
}

class Bar extends Foo {
	public function new(x:Int, y:Int) {
		super(x, y);
		var foo = new Foo(super.x, super.y);
	}
}