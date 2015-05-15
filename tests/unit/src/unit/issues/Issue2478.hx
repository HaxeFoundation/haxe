package unit.issues;

class Issue2478 extends Test {
    function test() {
			var o:Outcome<Int,String> = Success(5);

			switch (o) {
				case Success(v):
					t((v is Int));
				default:
					throw "assert";
			}
    }
}

private enum Outcome<D,F> {
	Success(data:D);
	Failure(error:F);
}
