import utest.Runner;
import utest.ui.Report;

class Main {
	static function main() {
		var runner = new Runner();
		//runner.addCases("cases");
		runner.addCase(new cases.Issue8063());
		var report = Report.create(runner);
		report.displayHeader = AlwaysShowHeader;
		report.displaySuccessResults = NeverShowSuccessResults;
		runner.run();
	}
}
