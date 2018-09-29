import hxbenchmark.SuiteResult;
@:autoBuild(Macro.buildCase())
class TestCase {
	var fieldFunctions:Array<() -> SuiteResult>;

	public function run(cb:SuiteResult -> Void) {
		for (f in fieldFunctions) {
			cb(f());
		}
	}
}