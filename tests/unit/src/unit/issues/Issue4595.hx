package unit.issues;

private enum Either<A> {
	Left(a:A);
}

class Issue4595 extends Test {
	function test() {
		var a = example(1); 
	}
	static function example<A>(a:A):Array<Array<Either<A>>>{
		return [];
	}
}
