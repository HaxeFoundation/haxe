import hxbenchmark.SuiteResult;
import hxbenchmark.ResultPrinter;

class Main {
	static function main() {
		var cases = Macro.getCases("cases");
		var printer = new ResultPrinter();
		function print(result:SuiteResult) {
			Sys.println(printer.print(result));
		}

		for (benchCase in cases) {
			Sys.println('Case: ${benchCase.name}');
			benchCase.exec.run(print);
		}
	}
}