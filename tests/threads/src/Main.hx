import utest.Runner;
import utest.ui.Report;

class Main {
	static function main() {
		utest.UTest.run([
			new cases.Issue4878(),
			new cases.DequeBrackets()
		]);
		/*
		var runner = new Runner();
		runner.addCases("cases");
		runner.onTestStart.add(test -> Sys.println("[START] " + test.fixture.target));
		runner.onTestComplete.add(test -> Sys.println("[STOP]  " + test.fixture.target));
		var report = Report.create(runner);
		report.displayHeader = AlwaysShowHeader;
		report.displaySuccessResults = NeverShowSuccessResults;
		runner.run();*/
	}
}
