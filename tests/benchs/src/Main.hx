import hxbenchmark.SuiteResult;
import hxbenchmark.ResultPrinter;

class Main {
	static function main() {
		var cases = Macro.getCases("cases");
		var printer = new ResultPrinter();
		function print(result:SuiteResult) {
			println(printer.print(result));
		}

		for (benchCase in cases) {
			println('Case: ${benchCase.name}');
			benchCase.exec.run(print);
		}
	}

	static public inline function println(msg:String) {
		#if sys
		Sys.println(msg);
		#elseif js
		js.Syntax.code('console.log({0})', msg);
		#else
		trace(msg);
		#end
	}
}