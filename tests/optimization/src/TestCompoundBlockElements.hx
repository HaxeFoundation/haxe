import TestJs.use;

class TestCompoundBlockElements {
	@:js('
		run(1);
		run(1);
		run(1);
	')
	static function testUnop() {
		-run(1);
		!run(1);
		~run(1);
	}

	@:js('
		run(1);
		run(2);
		run(3);
		run(4);
		run(5);
		run(6);
		run(7);
		run(8);
		run(9);
		run(10);
	')
	static function testBinop() {
		run(1) + run(2);
		run(3) - run(4);
		run(5) & run(6);
		run(7) == run(8);
		run(9) > run(10);
	}

	@:js('
		run(1);
		run(2);
	')
	static function testArrayAccess() {
		run(1)[run(2)];
	}

	@:js('
		run(1);
		run(2);
		run(3);
	')
	static function testArrayDeclaration() {
		[run(1), run(2)];
		run(3);
	}

	@:js('
		run(1);
		run(2);
		run(3);
	')
	static function testObjectDeclaration() {
		{
			a: run(1),
			b: run(2)
		};
		run(3);
	}

	@:pure(false)
	static inline function run(i:Int):Dynamic {
		return js.Syntax.code("run({0})", i);
	}
}