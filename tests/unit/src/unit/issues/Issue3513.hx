package unit.issues;

private enum Either<L, R> {
    Left(l:L);
    Right(r:R);
}

private abstract LazyGenerator<Data, End>(Void->Either<Data, End>) from Void->Either<Data, End> {
	public function next():Either<Data, End>
		return (this)();

	@:from static function infinite<Data, End>(f:Void->Data):LazyGenerator<Data, End>
		return function () return Left(f());
}

class Issue3513 extends Test {
	function test() {
        var count = 0;
        function counter() return count++;
        var gen:LazyGenerator<Int, Int> = counter;
        var gen:LazyGenerator<Int, Int> = function ():Int return count++;
		eq(0, getValue(gen.next()));
		eq(1, getValue(gen.next()));
	}

	static function getValue(e:Either<Int, Int>) {
		return switch (e) {
			case Left(v): v;
			case Right(v): v;
		}
	}
}