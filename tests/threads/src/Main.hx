import cases.WeirdTreeSum;
import utest.Runner;
import utest.ui.Report;

class Main {
	static function main() {
		var runner = new Runner();
		runner.addCase(new cases.WeirdTreeSum());
		#if !hl // no Lock
		#if !java // Deque broken?
		runner.addCase(new cases.DequeBrackets());
		#end
		#end
		var report = Report.create(runner);
		report.displayHeader = AlwaysShowHeader;
		report.displaySuccessResults = NeverShowSuccessResults;
		runner.run();
	}
}
