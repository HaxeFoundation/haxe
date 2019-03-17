import cases.WeirdTreeSum;
import utest.Runner;
import utest.ui.Report;

class Main {
	static function main() {
		var runner = new Runner();
		runner.addCase(new cases.WeirdTreeSum());
		runner.addCase(new cases.DequeBrackets());
		var report = Report.create(runner);
		report.displayHeader = AlwaysShowHeader;
		report.displaySuccessResults = NeverShowSuccessResults;
		runner.run();
	}
}
