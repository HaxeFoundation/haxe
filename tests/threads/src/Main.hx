import utest.Runner;
import utest.ui.Report;

class Main {
	static function main() {
		var runner = new Runner();
		runner.addCases("cases");
		var report = Report.create(runner);
		report.displayHeader = AlwaysShowHeader;
		report.displaySuccessResults = NeverShowSuccessResults;
		runner.run();
	}
}
